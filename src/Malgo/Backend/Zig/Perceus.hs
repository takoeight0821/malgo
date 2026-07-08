-- | The Perceus reference-counting pass (PLDI 2021, as adapted for
-- compiled closures by Lean 4's \"Counting Immutable Beans\"): a pure
-- 'Ir.Program' → 'Ir.Program' rewrite inserting 'Ir.Dup'\/'Ir.Drop' so
-- that, on every non-panic control-flow path, every owned reference is
-- consumed exactly once.
--
-- Ownership discipline (matching @runtime/zig/runtime.zig@):
--
--   * A function owns its parameters, and — for 'Ir.ClosureFn'\/
--     'Ir.FieldFn' — its @self@ closure object. Captures are borrowed
--     reads out of @self@ ('Ir.ReadCapture') promoted to owned by a 'Ir.Dup'
--     if live; @self@ is dropped as soon as it is dead (i.e. right after
--     the last capture read — the required dup-captures-before-drop-self
--     ordering falls out of plain liveness).
--   * 'Ir.MkStruct'\/'Ir.MkClosure'\/'Ir.MkRecord' move one reference per
--     operand into the new object; 'Ir.Force' moves one reference of the
--     record (the field function drops it as its @self@); every operand
--     of a consuming terminator moves into the call.
--   * 'Ir.Prim' operands, 'Ir.ReadPath'\/'Ir.ReadCapture' sources and
--     guard tests only borrow.
--
-- At every insertion point all 'Ir.Dup's precede any 'Ir.Drop' of the
-- same statement position (garbage-free ordering): a struct's live fields
-- are dup'd by their borrowed-let bindings before liveness kills the
-- struct itself.
module Malgo.Backend.Zig.Perceus (perceusProgram, perceusFunc) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Malgo.Backend.Zig.Ir
import Malgo.Prelude
import Malgo.Sequent.Fun (Name)

perceusProgram :: Program -> Program
perceusProgram program = program {funcs = map perceusFunc program.funcs}

perceusFunc :: Func -> Func
perceusFunc fn = fn {body = insertBlock delta0 fn.body}
  where
    -- A top-level function's @self@ is the immortal @rt.no_self@ sentinel,
    -- not an owned reference.
    delta0 =
      Set.fromList fn.params
        <> (if fn.kind == TopLevelFn then Set.empty else Set.singleton fn.selfVar)

-- | Insert RC operations into a block, given the set Δ of owned variables
-- in scope. Invariant: each Δ variable holds exactly one owned reference.
insertBlock :: Set Name -> Block -> Block
insertBlock delta (Block stmts term) =
  let (stmts', term') = goStmts delta stmts term
   in Block stmts' term'

goStmts :: Set Name -> [Stmt] -> Terminator -> ([Stmt], Terminator)
-- A bare panic block is RC-exempt (the process exits): no drops before it.
goStmts _ [] (TPanic msg) = ([], TPanic msg)
goStmts delta stmts term =
  -- (D) Eagerly drop every owned variable that is dead from here on.
  let live = freeVarsBlock (Block stmts term)
      deadNow = Set.toList (delta Set.\\ live)
      (rest, term') = goLive (delta `Set.intersection` live) stmts term
   in (map Drop deadNow <> rest, term')

goLive :: Set Name -> [Stmt] -> Terminator -> ([Stmt], Terminator)
goLive delta [] term = insertTerminator delta term
goLive delta (stmt : rest) term = case stmt of
  Dup _ -> error "Malgo.Backend.Zig.Perceus: input already contains Dup"
  Drop _ -> error "Malgo.Backend.Zig.Perceus: input already contains Drop"
  DropReuse {} -> error "Malgo.Backend.Zig.Perceus: input already contains DropReuse (Reuse runs after Perceus)"
  Let x e -> case e of
    -- noreturn: the rest of the block is unreachable, leave it untouched.
    PanicExpr _ -> (stmt : rest, term)
    ReadPath _ -> borrowedLet
    ReadCapture _ _ -> borrowedLet
    Lit _ -> owningLet []
    Prim _ _ -> owningLet []
    MkStruct _ ops -> owningLet ops
    MkClosure _ ops -> owningLet ops
    MkRecord _ ops -> owningLet ops
    Force v _ -> owningLet [v]
    MkStructReuse {} -> error "Malgo.Backend.Zig.Perceus: input already contains MkStructReuse (Reuse runs after Perceus)"
    where
      liveAfter = freeVarsBlock (Block rest term)
      -- A borrowed read creates no reference: promote the binding to owned
      -- with a Dup when it is live, elide it entirely when it is dead.
      borrowedLet
        | x `Set.member` liveAfter =
            let (rest', term') = goStmts (Set.insert x delta) rest term
             in (stmt : Dup x : rest', term')
        | otherwise = goStmts delta rest term
      -- (Let) The expression moves one reference per operand occurrence;
      -- an operand still live afterwards keeps its own reference, so it
      -- needs one Dup per occurrence, else one fewer (its reference moves).
      owningLet ops =
        let counts = Map.toList (Map.fromListWith (+) [(o, 1 :: Int) | o <- ops])
            dupsFor (y, n)
              | y `Set.notMember` delta =
                  error "Malgo.Backend.Zig.Perceus: consuming an unowned variable"
              | y `Set.member` liveAfter = replicate n (Dup y)
              | otherwise = replicate (n - 1) (Dup y)
            consumedAway = Set.fromList [y | (y, _) <- counts, y `Set.notMember` liveAfter]
            delta' = Set.insert x (delta Set.\\ consumedAway)
            (rest', term') = goStmts delta' rest term
         in (concatMap dupsFor counts <> (stmt : rest'), term')

insertTerminator :: Set Name -> Terminator -> ([Stmt], Terminator)
insertTerminator delta = \case
  -- (S) The guard only borrows; each branch drops (via goStmts's (D) rule)
  -- whatever is live only in the other branch.
  TIf guard t e -> ([], TIf guard (insertBlock delta t) (insertBlock delta e))
  TPanic msg -> ([], TPanic msg)
  -- (T) Consuming terminators: after the (D) rule, Δ is exactly the set of
  -- operands, each holding one reference; an operand occurring n times
  -- needs n − 1 extra references.
  term ->
    let counts = Map.toList (Map.fromListWith (+) [(o, 1 :: Int) | o <- termOperands term])
        dups = concat [replicate (n - 1) (Dup y) | (y, n) <- counts]
        _ = delta
     in (dups, term)
