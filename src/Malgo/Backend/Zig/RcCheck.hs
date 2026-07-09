-- | A linearity checker over the RC-annotated 'Ir.Program' produced by
-- 'Malgo.Backend.Zig.Perceus.perceusProgram': symbolic execution counting
-- each variable's owned references along every control-flow path,
-- reporting any path where a reference is consumed twice, used after
-- being consumed, or never consumed. Runs over the whole golden-test
-- corpus in hspec (no Zig toolchain needed) and as a compiler assertion
-- in @--debug-mode@.
--
-- The model is Perceus's local ownership discipline, deliberately: a
-- borrowed alias ('Ir.ReadPath'\/'Ir.ReadCapture' result) is accessible
-- only while its root still holds a reference in this scope (or the alias
-- was itself promoted by a 'Ir.Dup'). This is stricter than heap
-- reachability — exactly what makes it a useful oracle for the pass's
-- output rather than a general verifier.
--
-- Panic paths ('Ir.TPanic', 'Ir.PanicExpr') terminate the process and are
-- exempt from the consumed-exactly-once obligation.
module Malgo.Backend.Zig.RcCheck (checkProgram, checkFunc, RcViolation (..)) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Malgo.Backend.Zig.Ir
import Malgo.Prelude
import Malgo.Sequent.Fun (Name)

data RcViolation
  = -- | A variable was read or consumed on a path where its reference had
    -- already been moved away (or its borrow root died).
    UseAfterConsume Name Name
  | -- | Owned references still held when a consuming terminator fired.
    UnconsumedAtExit Name [Name]
  | -- | 'Ir.Dup' of a variable that is no longer accessible.
    DupOfDead Name Name
  | -- | 'Ir.Drop' of a variable holding no reference.
    DropOfDead Name Name
  | -- | 'Ir.MkStructReuse' referenced a reuse token not currently held
    -- (never produced by a 'Ir.DropReuse', or already consumed).
    TokenUnavailable Name Name
  | -- | Reuse tokens ('Ir.DropReuse') still held when a consuming
    -- terminator fired — every token must be consumed by exactly one
    -- 'Ir.MkStructReuse' before its statement list ends.
    TokenUnconsumed Name [Name]
  deriving stock (Eq, Show)

checkProgram :: Program -> Either [RcViolation] ()
checkProgram program = case concatMap checkFunc program.funcs of
  [] -> Right ()
  violations -> Left violations

data St = St
  { counts :: Map Name Int,
    aliasRoot :: Map Name Name,
    tokens :: Set Name
  }

checkFunc :: Func -> [RcViolation]
checkFunc fn = goBlock st0 fn.body
  where
    fname = fn.name
    st0 =
      St
        { counts =
            Map.fromList
              ( [(p, 1) | p <- fn.params]
                  <> [(fn.selfVar, 1) | fn.kind /= TopLevelFn]
              ),
          aliasRoot = Map.empty,
          tokens = Set.empty
        }

    ownedCount st x = Map.findWithDefault 0 x st.counts

    -- Accessible = holds a reference itself, or is a borrowed alias whose
    -- root (transitively) still does.
    accessible st x =
      ownedCount st x >= 1 || maybe False (accessible st) (Map.lookup x st.aliasRoot)

    useBorrowed st x = [UseAfterConsume fname x | not (accessible st x)]

    consume st x
      | ownedCount st x >= 1 = ([], st {counts = Map.adjust (subtract 1) x st.counts})
      | otherwise = ([UseAfterConsume fname x], st)

    consumeMany = foldl' (\(vs, st) x -> first (vs <>) (consume st x)) . ([],)

    bind st x n = st {counts = Map.insert x n st.counts}

    bindAlias st x root = (bind st x 0) {aliasRoot = Map.insert x root st.aliasRoot}

    goBlock st (Block stmts term) = goStmts st stmts term

    goStmts st [] term = goTerm st term
    goStmts st (stmt : rest) term = case stmt of
      Dup x
        | accessible st x -> goStmts st {counts = Map.insertWith (+) x 1 st.counts} rest term
        | otherwise -> DupOfDead fname x : goStmts st {counts = Map.insertWith (+) x 1 st.counts} rest term
      Drop x
        | ownedCount st x >= 1 -> goStmts st {counts = Map.adjust (subtract 1) x st.counts} rest term
        | otherwise -> DropOfDead fname x : goStmts st rest term
      DropReuse tok x _
        | ownedCount st x >= 1 ->
            goStmts st {counts = Map.adjust (subtract 1) x st.counts, tokens = Set.insert tok st.tokens} rest term
        | otherwise ->
            DropOfDead fname x : goStmts st {tokens = Set.insert tok st.tokens} rest term
      Let x e -> case e of
        -- noreturn: the path exits the process here; no obligations.
        PanicExpr _ -> []
        ReadPath p -> useBorrowed st (pathRoot p) <> goStmts (bindAlias st x (pathRoot p)) rest term
        ReadCapture self _ -> useBorrowed st self <> goStmts (bindAlias st x self) rest term
        Lit _ -> goStmts (bind st x 1) rest term
        Prim _ ops -> concatMap (useBorrowed st) ops <> goStmts (bind st x 1) rest term
        MkStruct _ ops -> consuming ops
        MkClosure _ ops -> consuming ops
        MkRecord _ ops -> consuming ops
        Force v _ -> consuming [v]
        MkStructReuse tok _ ops ->
          let tokViolation = [TokenUnavailable fname tok | tok `Set.notMember` st.tokens]
              st1 = st {tokens = Set.delete tok st.tokens}
              (vs, st2) = consumeMany st1 ops
           in tokViolation <> vs <> goStmts (bind st2 x 1) rest term
        where
          consuming ops =
            let (vs, st') = consumeMany st ops
             in vs <> goStmts (bind st' x 1) rest term

    goTerm st = \case
      TIf guard t e ->
        concatMap (useBorrowed st) (freeVarsGuard guard) <> goBlock st t <> goBlock st e
      TPanic _ -> []
      term ->
        let (vs, st') = consumeMany st (termOperands term)
            leftover = [y | (y, n) <- Map.toList st'.counts, n > 0]
            leftoverTokens = Set.toList st'.tokens
         in vs
              <> [UnconsumedAtExit fname leftover | not (null leftover)]
              <> [TokenUnconsumed fname leftoverTokens | not (null leftoverTokens)]
