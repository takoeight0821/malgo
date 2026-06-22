-- | The default set of lint rules.
--
-- @if@ and @case@ are ordinary Prelude functions, so every rule is a pure
-- AST-shape match. Rules run on the @Malgo Parse@ AST, where literals and the
-- raw @if@/@case@ forms survive.
module Malgo.Lint.Rules (allRules) where

import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Malgo.Lint.Diagnostic (warn)
import Malgo.Lint.Rule (Rule (..), exprRule)
import Malgo.Lint.Traversal (children, clauseRange)
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension (Malgo, MalgoPhase (Parse))

type P = Malgo Parse

allRules :: [Rule P]
allRules =
  [ caseOfBoundArg,
    singleBranchCase,
    redundantCaseForward,
    ifReturningBool,
    nestedIfEqualityChain
  ]

-- ── Shared matchers ─────────────────────────────────────────────────────────

-- | Strip a single-statement 'Seq' and 'Parens' down to the inner expression.
soleExpr :: Expr P -> Expr P
soleExpr (Seq _ (NoBind _ e :| [])) = soleExpr e
soleExpr (Parens _ e) = soleExpr e
soleExpr e = e

-- | The body of a thunk block @{ e }@ (a zero-arg @Fn@ with a wildcard clause).
thunkBody :: Expr P -> Maybe (Expr P)
thunkBody (Fn _ (Clause _ (VarP _ "_" :| []) body :| [])) = Just (soleExpr body)
thunkBody _ = Nothing

-- | Match @if c { t } { e }@, returning the condition and both branch bodies.
-- Matches the bare application only (no top-level 'Seq'/'Parens' unwrap), so
-- each occurrence is reported once at its 'Apply' node.
matchIf :: Expr P -> Maybe (Expr P, Expr P, Expr P)
matchIf (Apply _ (Apply _ (Apply _ (Var _ "if") c) tFn) eFn)
  | Just t <- thunkBody tFn,
    Just el <- thunkBody eFn =
      Just (c, t, el)
matchIf _ = Nothing

-- | Match @case scrut { clauses }@, returning the scrutinee and the @Fn@ of
-- clauses (kept as an 'Expr' so 'freevars' can be applied directly). Matches
-- the bare application only; callers unwrap a wrapping 'Seq' themselves.
matchCase :: Expr P -> Maybe (Expr P, Expr P)
matchCase (Apply _ (Apply _ (Var _ "case") scrut) fn@(Fn _ _)) = Just (scrut, fn)
matchCase _ = Nothing

-- ── Rule: case-of-bound-arg ─────────────────────────────────────────────────

-- @{ … x -> case x { … } }@ collapses to @{ … pat -> … }@ when @x@ is only
-- used as the scrutinee. Two shapes fire:
--
--   * @x@ is the last argument (the classic single-arg @case@ smell), or
--   * @x@ is bound by a /nested/ constructor, tuple, or list pattern, e.g.
--     @{ (Cons x xs) -> case x { … } }@, which folds into nested patterns
--     across extra clauses the same way.
--
-- A plain non-last @VarP@ parameter (@{ a x b -> case x { … } }@) is left
-- alone: folding it duplicates the other parameters across every clause for
-- little gain. A record-field binder (@{ {val = x} -> case x { … } }@) is
-- also left alone: the fold would push a constructor pattern into a record
-- field, which not every back end can match.
caseOfBoundArg :: Rule P
caseOfBoundArg = exprRule "case-of-bound-arg" check
  where
    check (Fn _ clauses) = concatMap clauseCheck (toList clauses)
    check _ = []
    clauseCheck c@(Clause _ pats body)
      | Just (scrut, fn) <- matchCase (soleExpr body),
        Var _ x <- soleExpr scrut,
        x `Set.notMember` freevars fn =
          case boundPosition x pats of
            LastParam -> [warn "case-of-bound-arg" (clauseRange c) ("`" <> x <> "` is bound only to be matched; drop `" <> x <> " -> case " <> x <> "` and match the argument directly.")]
            NestedBind -> [warn "case-of-bound-arg" (clauseRange c) ("`" <> x <> "` is bound by a pattern only to be matched; fold the `case` arms into the pattern that binds it.")]
            Elsewhere -> []
    clauseCheck _ = []

-- | Where a scrutinee variable is bound among a clause's head patterns.
data BoundPosition = LastParam | NestedBind | Elsewhere

boundPosition :: Text -> NonEmpty (Pat P) -> BoundPosition
boundPosition x pats
  | VarP _ x' <- NE.last pats, x == x' = LastParam
  | x `elem` topLevelVars = Elsewhere
  | x `Set.member` foldMap nestedFoldableVars pats = NestedBind
  | otherwise = Elsewhere
  where
    topLevelVars = [v | VarP _ v <- toList pats]

-- | Variables reachable through constructor\/tuple\/list nesting only —
-- substituting a pattern at these positions is a sound fold. Record-field
-- binders are excluded (folding would put a non-variable pattern in a record
-- field, which the self-hosted back end cannot match).
nestedFoldableVars :: Pat P -> Set Text
nestedFoldableVars (ConP _ _ ps) = foldMap nestedFoldableVars ps
nestedFoldableVars (TupleP _ ps) = foldMap nestedFoldableVars ps
nestedFoldableVars (ListP _ ps) = foldMap nestedFoldableVars ps
nestedFoldableVars (VarP _ x) = Set.singleton x
nestedFoldableVars _ = mempty

-- ── Rule: single-branch-case ────────────────────────────────────────────────

singleBranchCase :: Rule P
singleBranchCase = exprRule "single-branch-case" check
  where
    check e
      | Just (_, Fn _ (Clause _ (pat :| []) _ :| [])) <- matchCase e =
          case pat of
            VarP {} -> [warn "single-branch-case" (range e) "single-branch `case`; bind with `let` instead."]
            _ -> [warn "single-branch-case" (range e) "single-branch `case`; consider a `let` if the match is irrefutable."]
      | otherwise = []

-- ── Rule: redundant-case-forward ────────────────────────────────────────────

-- A branch that rebuilds its scrutinee unchanged, e.g. @Just v -> Just v@.
redundantCaseForward :: Rule P
redundantCaseForward = exprRule "redundant-case-forward" check
  where
    check e
      | Just (_, Fn _ clauses) <- matchCase e,
        any forwards (toList clauses) =
          [warn "redundant-case-forward" (range e) "a `case` branch rebuilds its scrutinee unchanged; consider an `orElse`/`maybe`-style combinator."]
      | otherwise = []
    forwards (Clause _ (ConP _ con [VarP _ v] :| []) body)
      | Apply _ (Var _ con') (Var _ v') <- soleExpr body = con == con' && v == v'
    forwards _ = False

-- ── Rule: if-returning-bool ─────────────────────────────────────────────────

ifReturningBool :: Rule P
ifReturningBool = exprRule "if-returning-bool" check
  where
    check e
      | Just (_, t, el) <- matchIf e =
          case (asBool t, asBool el) of
            (Just True, Just False) -> [warn "if-returning-bool" (range e) "`if c { True } { False }` is just `c`."]
            (Just False, Just True) -> [warn "if-returning-bool" (range e) "`if c { False } { True }` is just `not c`."]
            _ -> []
      | otherwise = []
    asBool (Var _ "True") = Just True
    asBool (Var _ "False") = Just False
    asBool _ = Nothing

-- ── Rule: nested-if-equality-chain ──────────────────────────────────────────

-- A right-nested chain of @if (eqX s lit) { … } { … }@ over the same scrutinee
-- should be a multi-clause function or a `case` over @s@. Uses custom recursion
-- so each maximal chain is reported once (at its head), not at every link.
nestedIfEqualityChain :: Rule P
nestedIfEqualityChain =
  Rule
    { ruleId = "nested-if-equality-chain",
      run = \decls -> concatMap scan [e | ScDef _ _ e <- decls]
    }
  where
    scan e =
      case chainLinks e Nothing of
        (n, thenBodies, dflt)
          | n >= 3 ->
              warn "nested-if-equality-chain" (range e) ("nested `if` chain of depth " <> tshow n <> " over `" <> scrutOf e <> "`; match it with multi-clause patterns or `case`.")
                : concatMap scan (thenBodies <> [dflt])
        _ -> concatMap scan (children e)

    -- Walk the maximal same-scrutinee eq-literal chain from @e@, returning
    -- (link count, then-branch bodies, final default body).
    chainLinks e mscrut =
      case matchIf e of
        Just (c, t, el)
          | Just s <- eqScrutLit c,
            maybe True (== s) mscrut ->
              let (n, ts, dflt) = chainLinks (soleExpr el) (Just s)
               in (n + 1, t : ts, dflt)
        _ -> (0 :: Int, [], e)

    scrutOf e = case matchIf e of
      Just (c, _, _) -> fromMaybe "?" (eqScrutLit c)
      _ -> "?"

    eqScrutLit c = case soleExpr c of
      Apply _ (Apply _ (Var _ fn) (Var _ s)) lit
        | fn `elem` eqFns, isLiteral (soleExpr lit) -> Just s
      _ -> Nothing

    isLiteral Boxed {} = True
    isLiteral Unboxed {} = True
    isLiteral _ = False

    eqFns :: [Text]
    eqFns = ["eqString", "eqChar", "eqInt32", "eqInt64"]

tshow :: (Show a) => a -> Text
tshow = convertString . show
