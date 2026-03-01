-- | Constraint-based Hindley-Milner type inference engine.
-- Uses level-based let-polymorphism and row polymorphism for records/variants.
module Malgo.Infer
  ( InferPass (..),
    InferError (..),
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.State.Static.Local (State, evalState, get)
import Malgo.Id (Id (..))
import Malgo.Infer.Constraint
import Malgo.Infer.Unify (solveConstraints)
import Malgo.Pass (Pass (..))
import Malgo.Prelude hiding (State, evalState, get, gets, modify, put)
import Malgo.Syntax
import Malgo.Syntax.Extension

-- | InferPass type-checks a renamed module using constraint-based HM inference.
data InferPass = InferPass

instance Pass InferPass where
  type Input InferPass = BindGroup (Malgo Rename)
  type Output InferPass = BindGroup (Malgo Rename)
  type ErrorType InferPass = InferError
  type
    Effects InferPass es =
      (State Uniq :> es)

  runPassImpl _ bindGroup = evalState initGenState do
    inferBindGroup bindGroup
    pure bindGroup

-- | Initial generation state
initGenState :: GenState
initGenState =
  GenState
    { nextVar = 0,
      constraints = [],
      currentLevel = 0,
      solvedSubst = Map.empty
    }

-- | Type environment mapping identifiers to type schemes
type TyEnv = Map Id Scheme

-- | Infer types for an entire bind group
inferBindGroup ::
  (State GenState :> es, Error InferError :> es) =>
  BindGroup (Malgo Rename) ->
  Eff es TyEnv
inferBindGroup bg = do
  -- Build initial environment from type signatures and data definitions
  let sigEnv = buildSigEnv bg
      dataEnv = buildDataEnv bg
      foreignEnv = buildForeignEnv bg
      env0 = sigEnv <> dataEnv <> foreignEnv

  -- Infer each mutually recursive group of definitions
  env <- foldlM inferScGroup env0 bg._scDefs

  -- Solve remaining constraints
  _ <- solveConstraints

  pure env

-- | Build type environment from type signatures
buildSigEnv :: BindGroup (Malgo Rename) -> TyEnv
buildSigEnv bg = Map.fromList $ map toSigEntry bg._scSigs
  where
    toSigEntry :: ScSig (Malgo Rename) -> (Id, Scheme)
    toSigEntry (_, name, ty) =
      let inferTy = surfaceTypeToTy ty
       in (name, Scheme {vars = [], ty = inferTy})

-- | Build type environment from data definitions (constructors)
buildDataEnv :: BindGroup (Malgo Rename) -> TyEnv
buildDataEnv bg = Map.fromList $ concatMap dataDefEntries bg._dataDefs
  where
    dataDefEntries :: DataDef (Malgo Rename) -> [(Id, Scheme)]
    dataDefEntries (_, typeName, params, cons) =
      let paramTys = map (\(_, p) -> TVar (idToVarName p) 0) params
          resultTy = foldl' TApp (TCon (idToVarName typeName)) paramTys
       in map (conEntry resultTy) cons

    conEntry :: Ty -> (Range, Id, [Type (Malgo Rename)]) -> (Id, Scheme)
    conEntry resultTy (_, conName, argTypes) =
      let argTys = map surfaceTypeToTy argTypes
          conTy = foldr TArr resultTy argTys
       in (conName, Scheme {vars = [], ty = conTy})

-- | Build type environment from foreign declarations
buildForeignEnv :: BindGroup (Malgo Rename) -> TyEnv
buildForeignEnv bg = Map.fromList $ map foreignEntry bg._foreigns
  where
    foreignEntry :: Foreign (Malgo Rename) -> (Id, Scheme)
    foreignEntry (_, name, ty) =
      (name, Scheme {vars = [], ty = surfaceTypeToTy ty})

-- | Infer a mutually recursive group of definitions
inferScGroup ::
  (State GenState :> es, Error InferError :> es) =>
  TyEnv ->
  [ScDef (Malgo Rename)] ->
  Eff es TyEnv
inferScGroup env defs = do
  enterLevel

  -- Create fresh type variables for each definition
  freshVars <- traverse (\(_, name, _) -> (name,) <$> freshTyVar) defs
  let localEnv = env <> Map.fromList (map (second (\ty -> Scheme [] ty)) freshVars)

  -- Generate constraints for each definition
  forM_ (zip defs freshVars) \((pos, _, expr), (_, expectedTy)) -> do
    actualTy <- inferExpr localEnv expr
    addConstraint $ CUnify pos expectedTy actualTy

  -- Solve constraints at this level
  _ <- solveConstraints

  exitLevel

  -- Generalize the types
  st <- get @GenState
  let finalEnv =
        foldl'
          ( \e (name, ty) ->
              let ty' = applySubst st.solvedSubst ty
                  scheme = generalize st.currentLevel ty'
               in Map.insert name scheme e
          )
          env
          freshVars

  pure finalEnv

-- | Infer the type of an expression
inferExpr ::
  (State GenState :> es, Error InferError :> es) =>
  TyEnv ->
  Expr (Malgo Rename) ->
  Eff es Ty
inferExpr env (Var pos name) =
  case Map.lookup name env of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ UnboundVariable pos name
inferExpr _ (Unboxed _ lit) = pure $ inferLiteral lit
inferExpr _ (Boxed pos _) = absurd pos
inferExpr env (Apply pos f arg) = do
  fTy <- inferExpr env f
  argTy <- inferExpr env arg
  retTy <- freshTyVar
  addConstraint $ CUnify pos fTy (TArr argTy retTy)
  pure retTy
inferExpr env (OpApp (pos, _) op lhs rhs) = do
  opTy <- case Map.lookup op env of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ UnboundVariable pos op
  lhsTy <- inferExpr env lhs
  rhsTy <- inferExpr env rhs
  retTy <- freshTyVar
  midTy <- freshTyVar
  addConstraint $ CUnify pos opTy (TArr lhsTy midTy)
  addConstraint $ CUnify pos midTy (TArr rhsTy retTy)
  pure retTy
inferExpr env (Project pos expr field) = do
  exprTy <- inferExpr env expr
  fieldTy <- freshTyVar
  rowTail <- freshTyVar
  addConstraint $ CUnify pos exprTy (TRecord [(field, fieldTy)] (Just rowTail))
  pure fieldTy
inferExpr env (Fn _ clauses) = do
  -- For simplicity, infer the first clause's type
  -- All clauses should have the same type
  let clauseList = toList clauses
  case clauseList of
    [] -> freshTyVar
    (c : cs) -> do
      ty <- inferClause env c
      -- Unify all clauses to have the same type
      forM_ cs \clause -> do
        clauseTy <- inferClause env clause
        -- Use a dummy range for clause unification
        addConstraint $ CUnify (dummyRange) ty clauseTy
      pure ty
inferExpr env (Tuple _ exprs) = do
  tys <- traverse (inferExpr env) exprs
  pure $ TTuple tys
inferExpr env (Record _ fields) = do
  fieldTys <- traverse (\(name, expr) -> (name,) <$> inferExpr env expr) fields
  pure $ TRecord fieldTys Nothing
inferExpr _ (List pos _) = absurd pos
inferExpr env (Ann pos expr ty) = do
  exprTy <- inferExpr env expr
  let annTy = surfaceTypeToTy ty
  addConstraint $ CUnify pos exprTy annTy
  pure annTy
inferExpr env (Seq _ stmts) = inferStmts env (toList stmts)
inferExpr env (Parens _ expr) = inferExpr env expr
inferExpr env (Codata _ clauses) = do
  resultTy <- freshTyVar
  forM_ clauses \(copat, expr) -> do
    inferCoClause env copat expr resultTy
  pure resultTy
inferExpr env (Label pos labelName body) = do
  -- Label introduces a continuation: the body can use goto to jump to the label
  resultTy <- freshTyVar
  let labelEnv = Map.insert labelName (Scheme [] resultTy) env
  bodyTy <- inferExpr labelEnv body
  addConstraint $ CUnify pos resultTy bodyTy
  pure resultTy
inferExpr env (Goto pos value label) = do
  -- Goto jumps to a label with a value; result type is bottom
  _ <- inferExpr env value
  _ <- inferExpr env label
  pure TBottom

-- | Infer the type of a clause (pattern matching branch)
inferClause ::
  (State GenState :> es, Error InferError :> es) =>
  TyEnv ->
  Clause (Malgo Rename) ->
  Eff es Ty
inferClause env (Clause _ pats body) = do
  -- Create fresh variables for each pattern
  (bindings, patTys) <- unzip <$> traverse (inferPat env) (toList pats)
  let localEnv = env <> Map.fromList (concat bindings)
  bodyTy <- inferExpr localEnv body
  pure $ foldr TArr bodyTy patTys

-- | Infer the type of a pattern, returning bindings and the pattern type
inferPat ::
  (State GenState :> es, Error InferError :> es) =>
  TyEnv ->
  Pat (Malgo Rename) ->
  Eff es ([(Id, Scheme)], Ty)
inferPat _ (VarP _ name) = do
  ty <- freshTyVar
  pure ([(name, Scheme [] ty)], ty)
inferPat env (ConP pos conName pats) = do
  case Map.lookup conName env of
    Just scheme -> do
      conTy <- instantiate scheme
      (bindings, patTys) <- unzip <$> traverse (inferPat env) pats
      resultTy <- freshTyVar
      let expectedTy = foldr TArr resultTy patTys
      addConstraint $ CUnify pos conTy expectedTy
      pure (concat bindings, resultTy)
    Nothing -> throwError $ UnboundVariable pos conName
inferPat env (TupleP _ pats) = do
  (bindings, patTys) <- unzip <$> traverse (inferPat env) pats
  pure (concat bindings, TTuple patTys)
inferPat env (RecordP _ fields) = do
  (bindings, fieldTys) <-
    unzip
      <$> traverse
        ( \(name, pat) -> do
            (b, ty) <- inferPat env pat
            pure (b, (name, ty))
        )
        fields
  pure (concat bindings, TRecord fieldTys Nothing)
inferPat _ (ListP pos _) = absurd pos
inferPat _ (UnboxedP _ lit) = pure ([], inferLiteral lit)
inferPat _ (BoxedP pos _) = absurd pos

-- | Infer a coclause (copattern matching)
inferCoClause ::
  (State GenState :> es, Error InferError :> es) =>
  TyEnv ->
  CoPat (Malgo Rename) ->
  Expr (Malgo Rename) ->
  Ty ->
  Eff es ()
inferCoClause env (HoleP _) body resultTy = do
  bodyTy <- inferExpr env body
  addConstraint $ CUnify (dummyRange) resultTy bodyTy
inferCoClause env (ApplyP pos copat pat) body resultTy = do
  (bindings, argTy) <- inferPat env pat
  retTy <- freshTyVar
  inferCoClause (env <> Map.fromList bindings) copat body retTy
  addConstraint $ CUnify pos resultTy (TArr argTy retTy)
inferCoClause env (ProjectP pos copat field) body resultTy = do
  fieldTy <- freshTyVar
  inferCoClause env copat body fieldTy
  rowTail <- freshTyVar
  addConstraint $ CUnify pos resultTy (TRecord [(field, fieldTy)] (Just rowTail))

-- | Infer the type of a sequence of statements
inferStmts ::
  (State GenState :> es, Error InferError :> es) =>
  TyEnv ->
  [Stmt (Malgo Rename)] ->
  Eff es Ty
inferStmts _ [] = pure tyUnit
inferStmts env [NoBind _ expr] = inferExpr env expr
inferStmts env (Let _ name expr : rest) = do
  enterLevel
  exprTy <- inferExpr env expr
  _ <- solveConstraints
  exitLevel
  st <- get @GenState
  let exprTy' = applySubst st.solvedSubst exprTy
      scheme = generalize st.currentLevel exprTy'
      env' = Map.insert name scheme env
  inferStmts env' rest
inferStmts env (With pos _ _ : rest) = do
  -- With statements are desugared before inference
  absurd pos
inferStmts env (NoBind _ expr : rest) = do
  _ <- inferExpr env expr
  inferStmts env rest

-- | Infer the type of a literal
inferLiteral :: Literal Unboxed -> Ty
inferLiteral (Int32 _) = tyInt32
inferLiteral (Int64 _) = tyInt64
inferLiteral (Float _) = tyFloat
inferLiteral (Double _) = tyDouble
inferLiteral (Char _) = tyChar
inferLiteral (String _) = tyString

-- | Convert surface syntax type to internal type
surfaceTypeToTy :: Type (Malgo Rename) -> Ty
surfaceTypeToTy (TyVar _ name) = TVar (idToVarName name) 0
surfaceTypeToTy (TyCon _ name) = TCon (idToVarName name)
surfaceTypeToTy (TyArr _ arg ret) = TArr (surfaceTypeToTy arg) (surfaceTypeToTy ret)
surfaceTypeToTy (TyApp _ f args) = foldl' TApp (surfaceTypeToTy f) (map surfaceTypeToTy args)
surfaceTypeToTy (TyTuple _ ts) = TTuple (map surfaceTypeToTy ts)
surfaceTypeToTy (TyRecord _ fields rowTail) =
  TRecord (map (second surfaceTypeToTy) fields) (surfaceTypeToTy <$> rowTail)
surfaceTypeToTy (TyBlock pos _) = absurd pos
surfaceTypeToTy (TyBottom _) = TBottom
surfaceTypeToTy (TyTilde _ t) = surfaceTypeToTy t
surfaceTypeToTy (TyVariant _ cases rowTail) =
  TVariant (map (second (map surfaceTypeToTy)) cases) (surfaceTypeToTy <$> rowTail)

-- | Convert an Id to a variable name for internal types
idToVarName :: Id -> T.Text
idToVarName Id {name} = name
