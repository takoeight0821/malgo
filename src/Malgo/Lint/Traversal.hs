-- | A 'Plated'-style traversal over the Malgo expression tree, built on
-- @microlens@ (no @lens@ dependency). 'plate' enumerates the immediate
-- sub-expressions of an expression — including those nested inside clauses,
-- statements and records — and 'universe'/'transform' are derived from it.
module Malgo.Lint.Traversal
  ( plate,
    children,
    universe,
    universeDecls,
    transform,
    clauseRange,
  )
where

import Lens.Micro (Traversal', toListOf)
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension (ForallClauseX)

-- | Traversal over the immediate sub-expressions of an expression.
plate :: Traversal' (Expr x) (Expr x)
plate f = \case
  Apply x a b -> Apply x <$> f a <*> f b
  OpApp x op a b -> OpApp x op <$> f a <*> f b
  Project x e k -> (\e' -> Project x e' k) <$> f e
  Fn x cs -> Fn x <$> traverse (clausePlate f) cs
  Tuple x es -> Tuple x <$> traverse f es
  Record x kvs -> Record x <$> traverse (\(k, v) -> (k,) <$> f v) kvs
  List x es -> List x <$> traverse f es
  Ann x e t -> (\e' -> Ann x e' t) <$> f e
  Seq x ss -> Seq x <$> traverse (stmtPlate f) ss
  Parens x e -> Parens x <$> f e
  Codata x cs -> Codata x <$> traverse (\(cp, e) -> (cp,) <$> f e) cs
  Label x n e -> Label x n <$> f e
  Goto x a b -> Goto x <$> f a <*> f b
  e@Var {} -> pure e
  e@Unboxed {} -> pure e
  e@Boxed {} -> pure e
  where
    clausePlate g (Clause x ps body) = Clause x ps <$> g body
    stmtPlate g = \case
      Let x n e -> Let x n <$> g e
      With x n e -> With x n <$> g e
      NoBind x e -> NoBind x <$> g e

-- | The immediate sub-expressions of an expression.
children :: Expr x -> [Expr x]
children = toListOf plate

-- | An expression and all of its transitive sub-expressions (self first).
universe :: Expr x -> [Expr x]
universe e = e : concatMap universe (children e)

-- | Every sub-expression of every top-level definition in a module.
universeDecls :: [Decl x] -> [Expr x]
universeDecls decls = concatMap universe [e | ScDef _ _ e <- decls]

-- | Bottom-up rewrite of every sub-expression. Unused by the report-only v1
-- rules, but the natural basis for a future @--fix@.
transform :: (Expr x -> Expr x) -> Expr x -> Expr x
transform f = f . over plate (transform f)

-- | Source range of a clause (no 'HasRange' instance exists for 'Clause').
clauseRange :: (ForallClauseX HasRange x) => Clause x -> Range
clauseRange (Clause x _ _) = range x
