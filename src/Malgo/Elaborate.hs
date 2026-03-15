-- | Codata elaboration pass.
-- Transforms copattern definitions into records and lambdas before
-- conversion to Fun IR.
--
-- Based on the ziku Elaborate algorithm:
--   1. Classify copatterns by first accessor (field vs. apply)
--   2. Field accessors → Record
--   3. Apply accessors → Lambda (Fn) with pattern matching
--   4. Reject mixed accessor kinds
module Malgo.Elaborate (ElaboratePass (..)) where

import Control.Exception (Exception)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Malgo.Id
import Malgo.Module
import Malgo.Pass (Pass (..))
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Prettyprinter ((<+>))

-- * Pass definition

data ElaboratePass = ElaboratePass

data ElaborateError
  = MixedAccessors Range
  | EmptyCopattern Range
  deriving stock (Eq)

instance Show ElaborateError where
  show = show . pretty

instance Pretty ElaborateError where
  pretty (MixedAccessors r) = pretty r <> ":" <+> "mixed field and call accessors in codata"
  pretty (EmptyCopattern r) = pretty r <> ":" <+> "empty copattern"

instance Exception ElaborateError

instance Pass ElaboratePass where
  type Input ElaboratePass = BindGroup (Malgo Rename)
  type Output ElaboratePass = BindGroup (Malgo Rename)
  type ErrorType ElaboratePass = ElaborateError
  type Effects ElaboratePass es = (State Uniq :> es, Reader ModuleName :> es)

  runPassImpl _ = elaborate

-- * Top-level elaboration

elaborate ::
  (State Uniq :> es, Reader ModuleName :> es, Error ElaborateError :> es) =>
  BindGroup (Malgo Rename) ->
  Eff es (BindGroup (Malgo Rename))
elaborate bg = do
  scDefs' <- traverse (traverse elabScDef) bg._scDefs
  pure bg {_scDefs = scDefs'}

elabScDef ::
  (State Uniq :> es, Reader ModuleName :> es, Error ElaborateError :> es) =>
  ScDef (Malgo Rename) ->
  Eff es (ScDef (Malgo Rename))
elabScDef (pos, name, expr) = do
  expr' <- elabExpr expr
  pure (pos, name, expr')

-- * Expression traversal

-- | Recursively elaborate all Codata expressions within an expression.
elabExpr ::
  (State Uniq :> es, Reader ModuleName :> es, Error ElaborateError :> es) =>
  Expr (Malgo Rename) ->
  Eff es (Expr (Malgo Rename))
elabExpr (Var pos name) = pure $ Var pos name
elabExpr (Unboxed pos lit) = pure $ Unboxed pos lit
elabExpr (Apply pos e1 e2) = Apply pos <$> elabExpr e1 <*> elabExpr e2
elabExpr (OpApp pos op e1 e2) = OpApp pos op <$> elabExpr e1 <*> elabExpr e2
elabExpr (Project pos e field) = (\e' -> Project pos e' field) <$> elabExpr e
elabExpr (Fn pos clauses) = Fn pos <$> traverse elabClause clauses
elabExpr (Tuple pos es) = Tuple pos <$> traverse elabExpr es
elabExpr (Record pos kvs) = Record pos <$> traverse (\(k, v) -> (k,) <$> elabExpr v) kvs
elabExpr (Ann pos e t) = (\e' -> Ann pos e' t) <$> elabExpr e
elabExpr (Seq pos stmts) = Seq pos <$> traverse elabStmt stmts
elabExpr (Parens pos e) = Parens pos <$> elabExpr e
elabExpr (Label pos name body) = Label pos name <$> elabExpr body
elabExpr (Goto pos value label) = Goto pos <$> elabExpr value <*> elabExpr label
elabExpr (Codata pos coclauses) = elabCodata pos coclauses

elabClause ::
  (State Uniq :> es, Reader ModuleName :> es, Error ElaborateError :> es) =>
  Clause (Malgo Rename) ->
  Eff es (Clause (Malgo Rename))
elabClause (Clause pos pats expr) = Clause pos pats <$> elabExpr expr

elabStmt ::
  (State Uniq :> es, Reader ModuleName :> es, Error ElaborateError :> es) =>
  Stmt (Malgo Rename) ->
  Eff es (Stmt (Malgo Rename))
elabStmt (Let pos v e) = Let pos v <$> elabExpr e
elabStmt (NoBind pos e) = NoBind pos <$> elabExpr e

-- * Copattern elaboration

-- | An accessor extracted from a flattened copattern.
data Accessor
  = FieldAcc Range Text
  | ApplyAcc Range (Pat (Malgo Rename))

-- | A clause for the copattern elaboration algorithm.
-- Contains the remaining accessors to process, any accumulated patterns
-- from stripped apply accessors, and the body expression.
data ElabClause = ElabClause
  { accessors :: [Accessor],
    accPatterns :: [Pat (Malgo Rename)],
    elabBody :: Expr (Malgo Rename)
  }

type Scrutinees = [Id]

-- | Flatten a CoPat tree into a list of accessors (left to right).
flattenCoPat :: CoPat (Malgo Rename) -> [Accessor]
flattenCoPat (HoleP _) = []
flattenCoPat (ApplyP pos copat pat) = flattenCoPat copat <> [ApplyAcc pos pat]
flattenCoPat (ProjectP pos copat field) = flattenCoPat copat <> [FieldAcc pos field]

-- | Entry point: elaborate a Codata expression.
elabCodata ::
  (State Uniq :> es, Reader ModuleName :> es, Error ElaborateError :> es) =>
  Range ->
  [CoClause (Malgo Rename)] ->
  Eff es (Expr (Malgo Rename))
elabCodata pos coclauses = do
  let clauses = map (\(copat, body) -> ElabClause (flattenCoPat copat) [] body) coclauses
  buildExpr [] pos clauses

-- | Recursively build an expression from elaboration clauses.
-- Dispatches based on the first accessor kind of the clauses.
buildExpr ::
  (State Uniq :> es, Reader ModuleName :> es, Error ElaborateError :> es) =>
  Scrutinees ->
  Range ->
  [ElabClause] ->
  Eff es (Expr (Malgo Rename))
buildExpr scrutinees pos clauses
  | all (\c -> null c.accessors) clauses = buildCase scrutinees pos clauses
  | all isField clauses = buildObject scrutinees pos clauses
  | all isApply clauses = buildLambda scrutinees pos clauses
  | otherwise = throwError (MixedAccessors pos)
  where
    isField (ElabClause (FieldAcc {} : _) _ _) = True
    isField _ = False
    isApply (ElabClause (ApplyAcc {} : _) _ _) = True
    isApply _ = False

-- | Build a pattern-matching expression for the base case (all accessors consumed).
-- Creates @(\\pats -> body) scrutinee@ for each clause.
buildCase ::
  (State Uniq :> es, Reader ModuleName :> es, Error ElaborateError :> es) =>
  Scrutinees ->
  Range ->
  [ElabClause] ->
  Eff es (Expr (Malgo Rename))
buildCase [] _ [ElabClause _ _ body] = elabExpr body
buildCase [] pos _ = throwError (EmptyCopattern pos)
buildCase scrutinees pos clauses = do
  fnClauses <- traverse mkFnClause clauses
  let scrutineeExpr = case scrutinees of
        [s] -> Var pos s
        ss -> Tuple pos (map (Var pos) ss)
  case NE.nonEmpty fnClauses of
    Just cs -> pure $ Apply pos (Fn pos cs) scrutineeExpr
    Nothing -> throwError (EmptyCopattern pos)
  where
    mkFnClause (ElabClause _ pats body) = do
      body' <- elabExpr body
      let pat = case pats of
            [p] -> p
            ps -> TupleP pos ps
      pure $ Clause pos (pat :| []) body'

-- | Build a lambda expression for apply accessors.
-- Strips the first apply accessor from each clause, generates a fresh
-- parameter, and recursively builds the body.
buildLambda ::
  (State Uniq :> es, Reader ModuleName :> es, Error ElaborateError :> es) =>
  Scrutinees ->
  Range ->
  [ElabClause] ->
  Eff es (Expr (Malgo Rename))
buildLambda scrutinees pos clauses = do
  param <- newTemporalId "elab"
  let clauses' = map stripApply clauses
  body <- buildExpr (scrutinees <> [param]) pos clauses'
  pure $ Fn pos (Clause pos (VarP pos param :| []) body :| [])
  where
    stripApply (ElabClause (ApplyAcc _ pat : rest) pats body) =
      ElabClause rest (pats <> [pat]) body
    stripApply _ = error "buildLambda: expected apply accessor"

-- | Build a record expression for field accessors.
-- Groups clauses by field name and recursively builds each field's expression.
buildObject ::
  (State Uniq :> es, Reader ModuleName :> es, Error ElaborateError :> es) =>
  Scrutinees ->
  Range ->
  [ElabClause] ->
  Eff es (Expr (Malgo Rename))
buildObject scrutinees pos clauses = do
  let grouped = groupByField clauses
  fields <- traverse (\(field, cs) -> (field,) <$> buildExpr scrutinees pos cs) grouped
  pure $ Record pos fields

-- | Group clauses by the field name of their first accessor, preserving order.
groupByField :: [ElabClause] -> [(Text, [ElabClause])]
groupByField = foldl' insert []
  where
    insert acc (ElabClause (FieldAcc _ field : rest) pats body) =
      insertGrouped field (ElabClause rest pats body) acc
    insert _ _ = error "groupByField: expected field accessor"
    insertGrouped field clause [] = [(field, [clause])]
    insertGrouped field clause ((f, cs) : rest)
      | f == field = (f, cs <> [clause]) : rest
      | otherwise = (f, cs) : insertGrouped field clause rest
