-- | Normalize eliminates two forms of pure aliasing from Join IR before
-- closure conversion, so ClosureConv never has to reason about them:
--
--   * @Cut (Mu x s) k@ binds @x@ to the covalue of @k@ and runs @s@ inline
--     (Eval.hs's special case for producer-position 'Mu'). Substituting
--     @x := k@ throughout @s@ is equivalent and removes 'Mu' entirely.
--   * @Join m (Label j) s@ makes @m@ a pure alias for @j@. Substituting
--     @m := j@ throughout @s@ removes the indirection.
--
-- Both eliminations are capture-avoiding for free because every 'Malgo.Id.Id'
-- in the pipeline is already globally unique.
module Malgo.Backend.Zig.Normalize (normalizeStatement, substStatement) where

import Malgo.Prelude
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Fun (Name)

-- | Substitute one 'Name' for another throughout a 'Statement'.
substStatement :: Name -> Name -> Statement -> Statement
substStatement from to = goS
  where
    r :: Name -> Name
    r n = if n == from then to else n

    goS (Cut p k) = Cut (goP p) (r k)
    goS (Join range name consumer stmt) =
      -- `name` is a fresh binder here; it can never equal `from` because
      -- `from` was already bound (and eliminated) further out.
      Join range name (goC consumer) (goS stmt)
    goS (Primitive range name ps k) = Primitive range name (map goP ps) (r k)
    goS (Invoke range name k) = Invoke range name (r k)
    goS (ExternalCall range name ps k) = ExternalCall range name (map goP ps) (r k)
    goS (BinOp range op lhs rhs k) = BinOp range op (goP lhs) (goP rhs) (r k)
    goS (Ifz range cond t e) = Ifz range (goP cond) (goS t) (goS e)

    goP (Var range n) = Var range (r n)
    goP lit@(Literal _ _) = lit
    goP (Construct range tag ps ks) = Construct range tag (map goP ps) (map r ks)
    goP (Lambda range names stmt) = Lambda range names (goS stmt)
    goP (Object range fields) = Object range (fmap (\(k, s) -> (k, goS s)) fields)
    goP (Mu range name stmt) = Mu range name (goS stmt)

    goC (Label range n) = Label range (r n)
    goC (Apply range ps ks) = Apply range (map goP ps) (map r ks)
    goC (Project range field k) = Project range field (r k)
    goC (Then range name stmt) = Then range name (goS stmt)
    goC f@(Finish _) = f
    goC (Select range branches) = Select range (map goB branches)

    goB (Branch range pat stmt) = Branch range pat (goS stmt)

-- | Eliminate 'Mu' (under 'Cut') and 'Label'-forwarding 'Join' bindings from
-- a 'Statement', bottom-up.
normalizeStatement :: Statement -> Statement
normalizeStatement = goS
  where
    goS (Cut p k) = case goP p of
      Mu _ x s -> normalizeStatement (substStatement x k s)
      p' -> Cut p' k
    goS (Join range name consumer stmt) = case goC consumer of
      Label _ j -> normalizeStatement (substStatement name j stmt)
      consumer' -> Join range name consumer' (goS stmt)
    goS (Primitive range name ps k) = Primitive range name (map goP ps) k
    goS (Invoke range name k) = Invoke range name k
    goS (ExternalCall range name ps k) = ExternalCall range name (map goP ps) k
    goS (BinOp range op lhs rhs k) = BinOp range op (goP lhs) (goP rhs) k
    goS (Ifz range cond t e) = Ifz range (goP cond) (goS t) (goS e)

    goP (Var range n) = Var range n
    goP lit@(Literal _ _) = lit
    goP (Construct range tag ps ks) = Construct range tag (map goP ps) ks
    goP (Lambda range names stmt) = Lambda range names (goS stmt)
    goP (Object range fields) = Object range (fmap (\(k, s) -> (k, goS s)) fields)
    goP (Mu range name stmt) = Mu range name (goS stmt)

    goC (Label range n) = Label range n
    goC (Apply range ps ks) = Apply range (map goP ps) ks
    goC (Project range field k) = Project range field k
    goC (Then range name stmt) = Then range name (goS stmt)
    goC f@(Finish _) = f
    goC (Select range branches) = Select range (map goB branches)

    goB (Branch range pat stmt) = Branch range pat (goS stmt)
