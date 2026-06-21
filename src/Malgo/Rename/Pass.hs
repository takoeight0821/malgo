-- | Name resolution and simple desugar transformation
module Malgo.Rename.Pass (RenamePass (..)) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static
import Effectful.Reader.Static (Reader, ask, asks, local, runReader)
import Effectful.State.Static.Local (State, execState, get, gets, modify, put, runState)
import Lens.Micro (_2)
import Malgo.Id
import Malgo.Interface
import Malgo.Module
import Malgo.Pass (Pass (..))
import Malgo.Prelude hiding (All)
import Malgo.Query (QueryDB, loadInterface)
import Malgo.Rename.RnEnv
import Malgo.Rename.RnState as RnState
import Malgo.Syntax
import Malgo.Syntax.Extension
import Prettyprinter (brackets, nest, squotes, vsep, (<+>))

data RenamePass = RenamePass

instance Pass RenamePass where
  type Input RenamePass = (Module (Malgo Parse), RnEnv)
  type Output RenamePass = (Module (Malgo Rename), RnState)
  type ErrorType RenamePass = RenameError
  type
    Effects RenamePass es =
      ( QueryDB :> es,
        State Uniq :> es,
        IOE :> es,
        Reader Flag :> es,
        Workspace :> es
      )

  runPassImpl _ (Module modName (ParsedDefinitions ds), builtinEnv) = do
    (ds', rnState) <- runState (RnState mempty Set.empty mempty mempty) $ runReader builtinEnv $ runReader modName $ rnDecls ds
    pure (Module modName $ makeBindGroup ds', rnState)

-- | Rename toplevel declarations
rnDecls ::
  ( Reader ModuleName :> es,
    Reader RnEnv :> es,
    State RnState :> es,
    QueryDB :> es,
    State Uniq :> es,
    IOE :> es,
    Reader Flag :> es,
    Workspace :> es,
    Error RenameError :> es
  ) =>
  [Decl (Malgo Parse)] ->
  Eff es [Decl (Malgo Rename)]
rnDecls ds = do
  -- RnEnvの生成
  rnEnv <- genToplevelEnv ds =<< ask
  local (const rnEnv) $ do
    -- RnStateの生成
    put =<< RnState <$> infixDecls ds <*> pure Set.empty <*> pure mempty <*> pure mempty
    -- 生成したRnEnv, RnStateの元でtraverse rnDecl ds
    traverse rnDecl ds

-- | Rename a toplevel declaration.
-- It is assumed that the top-level identifier defined in Decl has already been correctly registered in RnEnv.
-- The infix declaration is assumed to have already been interpreted and registered in RnState.
rnDecl ::
  ( State RnState :> es,
    QueryDB :> es,
    State Uniq :> es,
    Reader RnEnv :> es,
    Reader ModuleName :> es,
    IOE :> es,
    Reader Flag :> es,
    Workspace :> es,
    Error RenameError :> es
  ) =>
  Decl (Malgo Parse) ->
  Eff es (Decl (Malgo Rename))
rnDecl (ScDef pos name expr) = do
  resolvedName <- lookupVarName pos name
  registerExportedIdent resolvedName
  ScDef pos resolvedName <$> rnExpr expr
rnDecl (ScSig pos name typ) = do
  tyVars <- Set.toList <$> getTyVars typ
  tyVars' <- traverse resolveName tyVars
  resolvedName <- lookupVarName pos name
  registerExportedIdent resolvedName
  local (insertTypeIdent (zip tyVars $ map (Qualified Implicit) tyVars'))
    $ ScSig pos resolvedName
    <$> rnType typ
rnDecl (DataDef pos name params cs) = do
  params' <- traverse (resolveName . snd) params
  resolvedName <- lookupTypeName pos name
  registerExportedTypeIdent resolvedName
  local (insertTypeIdent (zip (map snd params) (map (Qualified Implicit) params')))
    $ DataDef pos resolvedName (zipWith (\(range, _) p' -> (range, p')) params params')
    <$> traverse
      ( bitraverse
          ( \cname -> do
              resolvedCName <- lookupVarName pos cname
              registerExportedIdent resolvedCName
              pure resolvedCName
          )
          (traverse rnType)
      )
      cs
rnDecl (TypeSynonym pos name params typ) = do
  params' <- traverse resolveName params
  resolvedName <- lookupTypeName pos name
  registerExportedTypeIdent resolvedName
  local (insertTypeIdent (zip params $ map (Qualified Implicit) params'))
    $ TypeSynonym pos resolvedName params'
    <$> rnType typ
rnDecl (Infix pos assoc prec name) = do
  resolvedName <- lookupVarName pos name
  registerExportedIdent resolvedName
  pure $ Infix pos assoc prec resolvedName
rnDecl (Foreign pos name typ) = do
  tyVars <- Set.toList <$> getTyVars typ
  tyVars' <- traverse resolveName tyVars
  resolvedName <- lookupVarName pos name
  registerExportedIdent resolvedName
  local (insertTypeIdent (zip tyVars $ map (Qualified Implicit) tyVars'))
    $ Foreign (pos, name) resolvedName
    <$> rnType typ
rnDecl (Import pos modName importList) = do
  interface <- loadInterface modName
  modify \s@RnState {..} ->
    s
      { RnState.infixInfo = s.infixInfo <> Map.mapKeys (externalFromInterface interface) interface.infixInfo,
        RnState.dependencies = Set.insert modName dependencies <> interface.dependencies,
        RnState.exportedIdentifiers = exportedIdentifiers
      }
  pure $ Import pos modName importList

-- | Get type variables from a type.
getTyVars :: Type (Malgo Parse) -> Eff es (Set Text)
getTyVars (TyApp _ t ts) = do
  t <- getTyVars t
  ts <- mconcat <$> traverse getTyVars ts
  pure $ t <> ts
getTyVars (TyVar _ v)
  | isUpper (T.head v) = pure mempty
  | otherwise = pure $ Set.singleton v
getTyVars (TyArr _ t1 t2) = getTyVars t1 <> getTyVars t2
getTyVars (TyTuple _ ts) = mconcat $ map getTyVars ts
getTyVars (TyRecord _ kvs rowTail) = mconcat (map (getTyVars . snd) kvs) <> maybe (pure mempty) getTyVars rowTail
getTyVars (TyBlock _ t) = getTyVars t
getTyVars (TyBottom _) = pure mempty
getTyVars (TyTilde _ t) = getTyVars t
getTyVars (TyVariant _ cases rowTail) = mconcat (map (foldMap getTyVars . snd) cases) <> maybe (pure mempty) getTyVars rowTail

-- | Rename a expression.
-- In addition to name resolution, OpApp recombination based on infix declarations is also performed.
rnExpr ::
  ( State RnState :> es,
    State Uniq :> es,
    Reader RnEnv :> es,
    Reader ModuleName :> es,
    IOE :> es,
    Reader Flag :> es,
    Error RenameError :> es
  ) =>
  Expr (Malgo Parse) ->
  Eff es (Expr (Malgo Rename))
rnExpr (Var range name) = Var range <$> lookupVarName range name
rnExpr (Unboxed pos val) = pure $ Unboxed pos val
rnExpr (Boxed pos val) = do
  f <- lookupBox pos val
  pure $ Apply pos (Var pos f) (Unboxed pos $ toUnboxed val)
rnExpr (Apply pos e1 e2) = Apply pos <$> rnExpr e1 <*> rnExpr e2
rnExpr (OpApp pos op e1 e2) = do
  op' <- lookupVarName pos op
  e1' <- rnExpr e1
  e2' <- rnExpr e2
  mfixity <- Map.lookup op' <$> gets @RnState (.infixInfo)
  case mfixity of
    Just fixity -> mkOpApp pos fixity op' e1' e2'
    Nothing -> errorOn pos $ "No infix declaration:" <+> squotes (pretty op)
rnExpr (Project range expr field) =
  case expr of
    Var vRange name -> do
      moduleNames <- asks @RnEnv (.moduleNames)
      if ModuleName name `Set.member` moduleNames
        then Var range <$> lookupQualifiedVarName range (ModuleName name) field
        else Project range <$> rnExpr (Var vRange name) <*> pure field
    _ -> Project range <$> rnExpr expr <*> pure field
rnExpr (Fn pos cs) = Fn pos <$> traverse rnClause cs
rnExpr (Tuple pos es) = Tuple pos <$> traverse rnExpr es
rnExpr (Record pos kvs) =
  Record pos
    <$> traverse
      ( bitraverse
          pure
          rnExpr
      )
      kvs
rnExpr (List pos es) = do
  nilName <- lookupVarName pos "Nil"
  consName <- lookupVarName pos "Cons"
  buildListApply nilName consName <$> traverse rnExpr es
  where
    buildListApply nilName _ [] = Var pos nilName
    buildListApply nilName consName (x : xs) = Apply pos (Apply pos (Var pos consName) x) (buildListApply nilName consName xs)
rnExpr (Ann pos e t) = Ann pos <$> rnExpr e <*> rnType t
rnExpr (Seq pos ss) = Seq pos <$> rnStmts ss
rnExpr (Parens pos e) = Parens pos <$> rnExpr e
rnExpr (Codata pos clauses) = Codata pos <$> traverse rnCoClause clauses
rnExpr (Label pos name body) = do
  name' <- resolveName name
  local (insertVarIdent [(name, Qualified Implicit name')])
    $ Label pos name'
    <$> rnExpr body
rnExpr (Goto pos value label) = Goto pos <$> rnExpr value <*> rnExpr label

-- | Renamed identifier corresponding Boxed literals.
lookupBox :: (Reader RnEnv :> es, Error RenameError :> es) => Range -> Literal x -> Eff es Id
lookupBox pos Int32 {} = lookupVarName pos "Int32#"
lookupBox pos Int64 {} = lookupVarName pos "Int64#"
lookupBox pos Float {} = lookupVarName pos "Float#"
lookupBox pos Double {} = lookupVarName pos "Double#"
lookupBox pos Char {} = lookupVarName pos "Char#"
lookupBox pos String {} = lookupVarName pos "String#"

-- | Rename a type.
rnType :: (Reader RnEnv :> es, IOE :> es, Reader Flag :> es, Error RenameError :> es) => Type (Malgo Parse) -> Eff es (Type (Malgo Rename))
rnType (TyApp pos t ts) = TyApp pos <$> rnType t <*> traverse rnType ts
rnType (TyVar pos x)
  | isUpper (T.head x) = TyCon pos <$> lookupTypeName pos x
  | otherwise = TyVar pos <$> lookupTypeName pos x
rnType (TyArr pos t1 t2) = TyArr pos <$> rnType t1 <*> rnType t2
rnType (TyTuple pos ts) = TyTuple pos <$> traverse rnType ts
rnType (TyRecord pos kts rowTail) = TyRecord pos <$> traverse (bitraverse pure rnType) kts <*> traverse rnType rowTail
rnType (TyBlock pos t) = TyArr pos (TyTuple pos []) <$> rnType t
rnType (TyBottom pos) = pure $ TyBottom pos
rnType (TyTilde pos t) = TyTilde pos <$> rnType t
rnType (TyVariant pos cases rowTail) = TyVariant pos <$> traverse (bitraverse pure (traverse rnType)) cases <*> traverse rnType rowTail

-- | Rename a clause.
rnClause ::
  ( State RnState :> es,
    State Uniq :> es,
    Reader RnEnv :> es,
    Reader ModuleName :> es,
    IOE :> es,
    Reader Flag :> es,
    Error RenameError :> es
  ) =>
  Clause (Malgo Parse) ->
  Eff es (Clause (Malgo Rename))
rnClause (Clause pos ps e) = do
  ps <- traverse resolveConP ps
  let vars = concatMap patVars ps
  -- パターンが束縛する変数に重複がないことを確認する
  -- TODO: throwError に置き換える
  when (hasDuplicates $ filter (/= "_") vars) $ errorOn pos "Same variables occurs in a pattern"
  vm <- zip vars . map (Qualified Implicit) <$> traverse resolveName vars
  local (insertVarIdent vm) $ Clause pos <$> traverse rnPat ps <*> rnExpr e

resolveConP :: Pat (Malgo Parse) -> Eff es (Pat (Malgo Parse))
resolveConP (VarP pos name)
  | isUpper (T.head name) = pure $ ConP pos name []
  | otherwise = pure $ VarP pos name
resolveConP (ConP pos name parameters) = do
  parameters' <- traverse resolveConP parameters
  pure $ ConP pos name parameters'
resolveConP (TupleP pos parameters) = do
  parameters' <- traverse resolveConP parameters
  pure $ TupleP pos parameters'
resolveConP (RecordP pos kvs) = do
  kvs' <- traverse (bitraverse pure resolveConP) kvs
  pure $ RecordP pos kvs'
resolveConP (ListP pos parameters) = do
  parameters' <- traverse resolveConP parameters
  pure $ ListP pos parameters'
resolveConP (UnboxedP pos x) = pure $ UnboxedP pos x
resolveConP (BoxedP pos x) = pure $ BoxedP pos x

patVars :: Pat x -> [XId x]
patVars (VarP _ x) = [x]
patVars (ConP _ _ xs) = concatMap patVars xs
patVars (TupleP _ xs) = concatMap patVars xs
patVars (RecordP _ kvs) = concatMap (patVars . snd) kvs
patVars (ListP _ xs) = concatMap patVars xs
patVars UnboxedP {} = []
patVars BoxedP {} = []

-- | Rename a pattern.
rnPat :: (Reader RnEnv :> es, IOE :> es, Reader Flag :> es, Error RenameError :> es) => Pat (Malgo Parse) -> Eff es (Pat (Malgo Rename))
rnPat (VarP pos x) = VarP pos <$> lookupVarName pos x
rnPat (ConP pos x xs) = ConP pos <$> lookupVarName pos x <*> traverse rnPat xs
rnPat (TupleP pos xs) = TupleP pos <$> traverse rnPat xs
rnPat (RecordP pos kvs) = RecordP pos <$> traverse (bitraverse pure rnPat) kvs
rnPat (ListP pos xs) = buildListP <$> lookupVarName pos "Nil" <*> lookupVarName pos "Cons" <*> traverse rnPat xs
  where
    buildListP nilName _ [] = ConP pos nilName []
    buildListP nilName consName (x : xs) = ConP pos consName [x, buildListP nilName consName xs]
rnPat (UnboxedP pos x) = pure $ UnboxedP pos x
rnPat (BoxedP pos x) = ConP pos <$> lookupBox pos x <*> pure [UnboxedP pos (coerce x)]

rnCoClause ::
  ( Reader RnEnv :> es,
    IOE :> es,
    Reader Flag :> es,
    Error RenameError :> es,
    State RnState :> es,
    State Uniq :> es,
    Reader ModuleName :> es
  ) =>
  (CoPat (Malgo Parse), Expr (Malgo Parse)) -> Eff es (CoPat (Malgo Rename), Expr (Malgo Rename))
rnCoClause (copat, expr) = do
  copat <- resolveConP' copat
  let vars = coPatVars copat
  -- パターンが束縛する変数に重複がないことを確認する
  -- TODO: throwError に置き換える
  when (hasDuplicates $ filter (/= "_") vars) $ errorOn (range copat) "Same variables occurs in a pattern"
  vm <- zip vars . map (Qualified Implicit) <$> traverse resolveName vars
  local (insertVarIdent vm) do
    copat <- rnCoPat copat
    expr <- rnExpr expr
    pure (copat, expr)
  where
    resolveConP' :: CoPat (Malgo Parse) -> Eff es (CoPat (Malgo Parse))
    resolveConP' (HoleP x) = pure $ HoleP x
    resolveConP' (ApplyP x copat pat) = ApplyP x <$> resolveConP' copat <*> resolveConP pat
    resolveConP' (ProjectP x copat field) = ProjectP x <$> resolveConP' copat <*> pure field
    coPatVars :: CoPat x -> [XId x]
    coPatVars (HoleP _) = []
    coPatVars (ApplyP _ copat pat) = coPatVars copat <> patVars pat
    coPatVars (ProjectP _ copat _) = coPatVars copat
    rnCoPat (HoleP x) = pure $ HoleP x
    rnCoPat (ApplyP x copat pat) = ApplyP x <$> rnCoPat copat <*> rnPat pat
    rnCoPat (ProjectP x copat field) = ProjectP x <$> rnCoPat copat <*> pure field

-- | Rename statements in {}.
rnStmts ::
  ( State RnState :> es,
    State Uniq :> es,
    Reader RnEnv :> es,
    Reader ModuleName :> es,
    IOE :> es,
    Reader Flag :> es,
    Error RenameError :> es
  ) =>
  NonEmpty (Stmt (Malgo Parse)) ->
  Eff es (NonEmpty (Stmt (Malgo Rename)))
rnStmts (NoBind x e :| []) = do
  e' <- rnExpr e
  pure $ NoBind x e' :| []
rnStmts (Let x v e :| []) = do
  e' <- rnExpr e
  v' <- resolveName v
  pure $ Let x v' e' :| []
rnStmts (NoBind x e :| s : ss) = do
  e' <- rnExpr e
  s' :| ss' <- rnStmts (s :| ss)
  pure $ NoBind x e' :| s' : ss'
rnStmts (Let x v e :| s : ss) = do
  e' <- rnExpr e
  v' <- resolveName v
  local (insertVarIdent [(v, Qualified Implicit v')]) do
    s' :| ss' <- rnStmts (s :| ss)
    pure $ Let x v' e' :| s' : ss'
rnStmts (LetP x pat e :| s : ss) = do
  -- desugar `let pat = e; rest` to `case e { pat -> rest }`,
  -- i.e. apply the single-clause lambda `{ pat -> rest }` to `e`.
  e <- rnExpr e
  k <- rnExpr (Fn x $ Clause x (NE.singleton pat) (Seq x $ s :| ss) :| [])
  pure $ NoBind x (Apply x k e) :| []
rnStmts (LetP x _ _ :| []) = errorOn x "`let` binding a pattern cannot appear in the last line of the sequence expression."
rnStmts (With x (Just v) e :| s : ss) = do
  e <- rnExpr e
  ss <- rnExpr (Fn x $ Clause x (NE.singleton (VarP x v)) (Seq x $ s :| ss) :| [])
  pure $ NoBind x (Apply x e ss) :| []
rnStmts (With x Nothing e :| s : ss) = do
  e <- rnExpr e
  ss <- rnExpr (Fn x $ Clause x (NE.singleton (VarP x "_")) (Seq x $ s :| ss) :| [])
  pure $ NoBind x (Apply x e ss) :| []
rnStmts (With x _ _ :| []) = errorOn x "`with` statement cannnot appear in the last line of the sequence expression."

-- | Convert infix declarations to a Map. Infix for an undefined identifier is an error.
infixDecls :: (Reader RnEnv :> es, Error RenameError :> es) => [Decl (Malgo Parse)] -> Eff es (Map RnId (Assoc, Int))
infixDecls ds =
  foldMapM
    ( \case
        (Infix pos assoc order name) -> do
          name' <- lookupVarName pos name
          pure $ Map.singleton name' (assoc, order)
        _ -> pure mempty
    )
    ds

-- | OpApp recombination.
-- Every OpApp in 'Malgo Parse' is treated as left associative.
-- 'mkOpApp' transforms it to actual associativity.
mkOpApp ::
  (IOE :> es, Reader Flag :> es) =>
  Range ->
  -- | Fixity of outer operator
  (Assoc, Int) ->
  -- | Outer operator
  RnId ->
  -- | Left expression as (x op y)
  Expr (Malgo Rename) ->
  -- | Right expression
  Expr (Malgo Rename) ->
  Eff es (Expr (Malgo Rename))
-- (e11 op1 e12) op2 e2
mkOpApp pos2 fix2 op2 (OpApp (pos1, fix1) op1 e11 e12) e2
  | nofix_error =
      errorOn pos1
        $ vsep
          [ "Precedence parsing error:",
            nest
              2
              ( "cannot mix"
                  <+> squotes (pretty op1)
                  <+> brackets (pretty fix1)
                  <+> "and"
                  <+> squotes (pretty op2)
                  <+> brackets (pretty fix2)
                  <+> "in the same infix expression"
              )
          ]
  | associate_right = do
      e' <- mkOpApp pos2 fix2 op2 e12 e2
      pure $ OpApp (pos1, fix1) op1 e11 e'
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2
    compareFixity (assoc1, prec1) (assoc2, prec2) = case prec1 `compare` prec2 of
      GT -> left
      LT -> right
      EQ -> case (assoc1, assoc2) of
        (RightA, RightA) -> right
        (LeftA, LeftA) -> left
        _ -> error_please
      where
        right = (False, True)
        left = (False, False)
        error_please = (True, False)
mkOpApp pos fix op e1 e2 = pure $ OpApp (pos, fix) op e1 e2

-- | Generate toplevel environment.
genToplevelEnv :: (IOE :> es, Reader ModuleName :> es, QueryDB :> es, Workspace :> es, Error RenameError :> es) => [Decl (Malgo Parse)] -> RnEnv -> Eff es RnEnv
genToplevelEnv (ds :: [Decl (Malgo Parse)]) env = do
  execState env (traverse aux ds)
  where
    -- Only an existing *Implicit* (same-module) binding counts as a true duplicate.
    -- Names imported via 'module Alias = import ...' carry Explicit visibility
    -- and must not prevent the current module from defining a same-named identifier.
    hasImplicitBinding name m = any (\(Qualified visi _) -> visi == Implicit) $ fromMaybe [] $ Map.lookup name m
    aux (ScDef pos x _) = do
      env <- gets @RnEnv (.resolvedVarIdentMap)
      when (hasImplicitBinding x env) do
        throwError $ DuplicateName pos x
      x' <- resolveGlobalName x
      modify $ insertVarIdent [(x, Qualified Implicit x')]
    aux ScSig {} = pass
    aux (DataDef pos x _ cs) = do
      env <- get @RnEnv
      when (hasImplicitBinding x env.resolvedTypeIdentMap) do
        throwError $ DuplicateName pos x
      let conConflicts = filter (\c -> hasImplicitBinding c env.resolvedVarIdentMap) (map (view _2) cs)
      unless (null conConflicts) do
        throwError $ DuplicateNames pos conConflicts
      x' <- resolveGlobalName x
      xs' <- traverse (resolveGlobalName . view _2) cs
      modify $ insertVarIdent (zip (map (view _2) cs) $ map (Qualified Implicit) xs')
      modify $ addConstructors xs'
      modify $ insertTypeIdent [(x, Qualified Implicit x')]
    aux (TypeSynonym pos x _ _) = do
      env <- get @RnEnv
      when (hasImplicitBinding x env.resolvedTypeIdentMap) do
        throwError $ DuplicateName pos x
      x' <- resolveGlobalName x
      modify $ insertTypeIdent [(x, Qualified Implicit x')]
    aux (Foreign pos x _) = do
      env <- get @RnEnv
      when (hasImplicitBinding x env.resolvedVarIdentMap) do
        throwError $ DuplicateName pos x
      x' <- resolveGlobalName x
      modify $ insertVarIdent [(x, Qualified Implicit x')]
    aux (Import _ modName' importList) = do
      -- If imported variables are already defined in the current module, shadow them.
      interface <- loadInterface modName'
      let varIdentAssoc =
            map
              (\name -> (name, externalFromInterface interface name))
              (exportedIdentList interface)
      let typeIdentAssoc =
            map
              (\name -> (name, externalFromInterface interface name))
              (exportedTypeIdentList interface)
      modify $ insertVarIdent (map (resolveImport modName' importList) varIdentAssoc)
      modify $ insertTypeIdent (map (resolveImport modName' importList) typeIdentAssoc)
      case importList of
        As moduleName -> modify \s -> s {moduleNames = Set.insert moduleName s.moduleNames}
        _ -> pass
    aux Infix {} = pass

resolveImport :: ModuleName -> ImportList -> (PsId, RnId) -> (PsId, Qualified RnId)
resolveImport _ All (psId, rnId) = (psId, Qualified Implicit rnId)
resolveImport modName (Selected implicits) (psId, rnId)
  | psId `elem` implicits = (psId, Qualified Implicit rnId)
  | otherwise = (psId, Qualified (Explicit modName) rnId)
resolveImport _ (As modNameAs) (psId, rnId) = (psId, Qualified (Explicit modNameAs) rnId)

-- | Register an identifier as exported if it should be exported from the current module
registerExportedIdent :: (State RnState :> es, Reader ModuleName :> es) => Id -> Eff es ()
registerExportedIdent ident = do
  currentModule <- ask @ModuleName
  when (ident.sort == External && ident.moduleName == currentModule) do
    modify \s@RnState {..} ->
      s {exportedIdentifiers = ident.name : exportedIdentifiers}

-- | Register a type identifier as exported if it should be exported from the current module
registerExportedTypeIdent :: (State RnState :> es, Reader ModuleName :> es) => Id -> Eff es ()
registerExportedTypeIdent ident = do
  currentModule <- ask @ModuleName
  when (ident.sort == External && ident.moduleName == currentModule) do
    modify \s@RnState {..} ->
      s {exportedTypeIdentifiers = ident.name : exportedTypeIdentifiers}
