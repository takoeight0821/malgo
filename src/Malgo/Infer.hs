-- | Constraint-based Hindley-Milner type inference engine.
-- Uses level-based let-polymorphism and row polymorphism for records/variants.
module Malgo.Infer
  ( InferPass (..),
    InferError (..),
    TyEnv,
    buildSigEnv,
    buildDataEnv,
    buildForeignEnv,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.State.Static.Local (State, evalState, get)
import Malgo.Features (Feature (Experimental), Features, hasFeature)
import Malgo.Id (Id (..))
import Malgo.Infer.Constraint
import Malgo.Infer.Unify (solveConstraints)
import Malgo.Module (ModuleName)
import Malgo.Pass (Pass (..))
import Malgo.Prelude hiding (State, evalState, get, gets, modify, put)
import Malgo.Syntax
import Malgo.Syntax.Extension

-- | InferPass type-checks a renamed module using constraint-based HM inference.
data InferPass = InferPass

instance Pass InferPass where
  type Input InferPass = (TyEnv, BindGroup (Malgo Rename))
  type Output InferPass = (BindGroup (Malgo Rename), TyEnv)
  type ErrorType InferPass = InferError
  type
    Effects InferPass es =
      ( State Uniq :> es,
        Reader ModuleName :> es,
        Features :> es
      )

  runPassImpl _ (importedEnv, bindGroup) = do
    -- Checks experimental iso-recursive unification feature flag.
    useIso <- hasFeature (Experimental "iso-recursive-unify")
    let mode = if useIso then IsoRecursive else EquiRecursive
    evalState (initGenState mode) do
      finalEnv <- inferBindGroup importedEnv bindGroup
      pure (bindGroup, finalEnv)

-- | Initial generation state
initGenState :: RecursionMode -> GenState
initGenState mode =
  GenState
    { constraints = [],
      currentLevel = 0,
      solvedSubst = Map.empty,
      recursionMode = mode
    }

-- | Type environment mapping identifiers to type schemes
type TyEnv = Map Id Scheme

-- | Type-synonym environment built from 'BindGroup._typeSynonyms'.
-- The right-hand side is stored unexpanded; it is unfolded on demand by
-- 'surfaceTypeToTy' so that recursive synonyms remain representable until
-- they are actually used in a type position.
type SynEnv = Map Id ([Id], Type (Malgo Rename))

-- | Build the synonym environment from a bind group.
buildSynEnv :: BindGroup (Malgo Rename) -> SynEnv
buildSynEnv bg =
  Map.fromList
    [ (name, (params, body))
    | (_, name, params, body) <- bg._typeSynonyms
    ]

-- | Effect bundle required by the constraint generators. 'State Uniq' /
-- 'Reader ModuleName' are needed because 'freshTyVar' (and any helper that
-- transitively calls it, e.g. 'instantiate' / 'solveConstraints' /
-- 'inferExpr Ann' via the unifier's 'TForall' case) generates fresh 'Id's.
type InferEffs es =
  ( Reader SynEnv :> es,
    Reader ModuleName :> es,
    State GenState :> es,
    State Uniq :> es,
    Error InferError :> es
  )

-- | Infer types for an entire bind group
inferBindGroup ::
  ( State GenState :> es,
    State Uniq :> es,
    Reader ModuleName :> es,
    Error InferError :> es
  ) =>
  TyEnv ->
  BindGroup (Malgo Rename) ->
  Eff es TyEnv
inferBindGroup importedEnv bg = runReader (buildSynEnv bg) do
  -- Build initial environment from type signatures and data definitions
  sigEnv <- buildSigEnv bg
  dataEnv <- buildDataEnv bg
  foreignEnv <- buildForeignEnv bg
  let env0 = importedEnv <> sigEnv <> dataEnv <> foreignEnv

  -- Infer each mutually recursive group of definitions
  env <- foldlM inferScGroup env0 bg._scDefs

  -- Solve remaining constraints
  _ <- solveConstraints

  pure env

-- | Build type environment from type signatures
buildSigEnv ::
  (Reader SynEnv :> es, Error InferError :> es) =>
  BindGroup (Malgo Rename) ->
  Eff es TyEnv
buildSigEnv bg = Map.fromList <$> traverse toSigEntry bg._scSigs
  where
    toSigEntry :: (Reader SynEnv :> es, Error InferError :> es) => ScSig (Malgo Rename) -> Eff es (Id, Scheme)
    toSigEntry (_, name, ty) = do
      inferTy <- surfaceTypeToTy ty
      let fvs = Set.toList $ freeVars inferTy
      pure (name, Scheme {vars = fvs, ty = inferTy})

-- | Build type environment from data definitions (constructors)
buildDataEnv ::
  (Reader SynEnv :> es, Error InferError :> es) =>
  BindGroup (Malgo Rename) ->
  Eff es TyEnv
buildDataEnv bg = Map.fromList . concat <$> traverse dataDefEntries bg._dataDefs
  where
    dataDefEntries :: (Reader SynEnv :> es, Error InferError :> es) => DataDef (Malgo Rename) -> Eff es [(Id, Scheme)]
    dataDefEntries (_, typeName, params, cons) = do
      let paramTys = map (\(_, p) -> TVar p 0) params
          resultTy = foldl' TApp (TCon typeName.name) paramTys
      traverse (conEntry resultTy) cons

    conEntry :: (Reader SynEnv :> es, Error InferError :> es) => Ty -> (Range, Id, [Type (Malgo Rename)]) -> Eff es (Id, Scheme)
    conEntry resultTy (_, conName, argTypes) = do
      argTys <- traverse surfaceTypeToTy argTypes
      let conTy = foldr TArr resultTy argTys
          fvs = Set.toList $ freeVars conTy
      pure (conName, Scheme {vars = fvs, ty = conTy})

-- | Build type environment from foreign declarations
buildForeignEnv ::
  (Reader SynEnv :> es, Error InferError :> es) =>
  BindGroup (Malgo Rename) ->
  Eff es TyEnv
buildForeignEnv bg = Map.fromList <$> traverse foreignEntry bg._foreigns
  where
    foreignEntry :: (Reader SynEnv :> es, Error InferError :> es) => Foreign (Malgo Rename) -> Eff es (Id, Scheme)
    foreignEntry (_, name, ty) = do
      inferTy <- surfaceTypeToTy ty
      let fvs = Set.toList $ freeVars inferTy
      pure (name, Scheme {vars = fvs, ty = inferTy})

-- | Infer a mutually recursive group of definitions
inferScGroup ::
  (InferEffs es) =>
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
  (InferEffs es) =>
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
  annTy <- surfaceTypeToTy ty
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
  (InferEffs es) =>
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
  (InferEffs es) =>
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
inferPat env (RecordP pos fields) = do
  (bindings, fieldTys) <-
    unzip
      <$> traverse
        ( \(name, pat) -> do
            (b, ty) <- inferPat env pat
            pure (b, (name, ty))
        )
        fields
  resultTy <- freshTyVar
  addConstraint $ CUnify pos resultTy (TRecord fieldTys Nothing)
  pure (concat bindings, resultTy)
inferPat _ (ListP pos _) = absurd pos
inferPat _ (UnboxedP _ lit) = pure ([], inferLiteral lit)
inferPat _ (BoxedP pos _) = absurd pos

-- | Infer a coclause (copattern matching)
inferCoClause ::
  (InferEffs es) =>
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
  (InferEffs es) =>
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

-- | Convert surface syntax type to internal type, eagerly expanding type
-- synonyms found in the ambient 'SynEnv'. Recursive expansion is detected
-- via a visited set; partial application of a synonym is rejected.
surfaceTypeToTy ::
  (Reader SynEnv :> es, Error InferError :> es) =>
  Type (Malgo Rename) ->
  Eff es Ty
surfaceTypeToTy = expandType Set.empty

expandType ::
  (Reader SynEnv :> es, Error InferError :> es) =>
  Set Id ->
  Type (Malgo Rename) ->
  Eff es Ty
expandType _ (TyVar _ name) = pure $ TVar name 0
expandType visited (TyCon pos name) = do
  synEnv <- ask @SynEnv
  case Map.lookup name synEnv of
    Just (params, _)
      | not (null params) ->
          throwError $ SynonymArityMismatch pos name (length params) 0
    Just (_, body) -> do
      when (Set.member name visited) $ throwError (CyclicSynonym pos name)
      expandType (Set.insert name visited) body
    Nothing -> pure $ TCon name.name
expandType visited (TyArr _ arg ret) =
  TArr <$> expandType visited arg <*> expandType visited ret
expandType visited (TyApp pos f args) = do
  synEnv <- ask @SynEnv
  case f of
    TyCon _ name | Just (params, body) <- Map.lookup name synEnv -> do
      when (length params /= length args)
        $ throwError
        $ SynonymArityMismatch pos name (length params) (length args)
      argTys <- traverse (expandType visited) args
      expandSynonymApp pos visited name params argTys body
    _ -> foldl' TApp <$> expandType visited f <*> traverse (expandType visited) args
expandType visited (TyTuple _ ts) = TTuple <$> traverse (expandType visited) ts
expandType visited (TyRecord _ fields rowTail) =
  TRecord
    <$> traverse (\(n, t) -> (n,) <$> expandType visited t) fields
    <*> traverse (expandType visited) rowTail
expandType _ (TyBlock pos _) = absurd pos
expandType _ (TyBottom _) = pure TBottom
expandType visited (TyTilde _ t) = expandType visited t
expandType visited (TyVariant _ cases rowTail) =
  TVariant
    <$> traverse (\(n, ts) -> (n,) <$> traverse (expandType visited) ts) cases
    <*> traverse (expandType visited) rowTail

-- | Expand a parameterised synonym application by substituting arg types
-- into the body's free type variables, after recursively converting the body.
-- Because 'Ty.TVar' carries an 'Id' (and thus uniqueness across scopes),
-- 'applySubst' is safe here even when a synonym's parameter shares its
-- surface name with an outer-scope variable.
expandSynonymApp ::
  (Reader SynEnv :> es, Error InferError :> es) =>
  Range ->
  Set Id ->
  Id ->
  [Id] ->
  [Ty] ->
  Type (Malgo Rename) ->
  Eff es Ty
expandSynonymApp pos visited name params argTys body = do
  when (Set.member name visited) $ throwError (CyclicSynonym pos name)
  bodyTy <- expandType (Set.insert name visited) body
  let subst = Map.fromList (zip params argTys)
  pure $ applySubst subst bodyTy
