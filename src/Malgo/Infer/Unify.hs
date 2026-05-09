-- | Unification algorithm with row polymorphism support.
-- Handles record/variant row variables and bottom type propagation.
module Malgo.Infer.Unify
  ( unify,
    solveConstraints,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State, evalState, get, modify)
import Malgo.Infer.Constraint
import Malgo.Module (ModuleName)
import Malgo.Prelude hiding (Constraint)

-- | Effect bundle required by the unifier and constraint solver.
-- 'State Uniq' / 'Reader ModuleName' come from 'freshTyVar'
-- (used when instantiating 'TForall' on the fly).
type UnifyEffs es =
  ( State GenState :> es,
    State Uniq :> es,
    Reader ModuleName :> es,
    Error InferError :> es
  )

-- | Unify two types, returning a substitution
unify :: (UnifyEffs es) => Range -> Ty -> Ty -> Eff es Subst
unify pos t1 t2 = do
  subst <- currentSubst
  let t1' = applySubst subst t1
      t2' = applySubst subst t2
  s <- evalState (Set.empty :: Set (Ty, Ty)) $ unifyTypes pos t1' t2'
  commitSubst s
  pure s

-- | Core unification
unifyTypes :: (UnifyEffs es, State (Set (Ty, Ty)) :> es) => Range -> Ty -> Ty -> Eff es Subst
unifyTypes pos t1 t2
  | t1 == t2 = pure Map.empty
  | otherwise = do
      seen <- get @(Set (Ty, Ty))
      if Set.member (t1, t2) seen || Set.member (t2, t1) seen
        then pure Map.empty
        else do
          modify @(Set (Ty, Ty)) (Set.insert (t1, t2))
          unifyInternal pos t1 t2

unifyInternal :: (UnifyEffs es, State (Set (Ty, Ty)) :> es) => Range -> Ty -> Ty -> Eff es Subst
-- Bottom unifies with anything
unifyInternal _ TBottom _ = pure Map.empty
unifyInternal _ _ TBottom = pure Map.empty
-- Type constructors
unifyInternal pos (TCon c1) (TCon c2)
  | c1 == c2 = pure Map.empty
  | otherwise = throwError $ UnificationError pos (TCon c1) (TCon c2) $ "Cannot unify type constructors '" <> c1 <> "' and '" <> c2 <> "'"
-- Same variable
unifyInternal _ (TVar x _) (TVar y _)
  | x == y = pure Map.empty
-- Variable on the left
unifyInternal _ (TVar x _) t
  | occursIn x t = do
      -- Equi-recursive unification: create TMu
      let t' = TMu x t
      pure $ Map.singleton x t'
  | otherwise = do
      pure $ Map.singleton x t
-- Variable on the right
unifyInternal pos t (TVar x l) = unifyTypes pos (TVar x l) t
-- TMu on the left: unroll and unify
unifyInternal pos (TMu v ty) t2 = do
  let unrolled = applySubst (Map.singleton v (TMu v ty)) ty
  unifyTypes pos unrolled t2
-- TMu on the right: unroll and unify
unifyInternal pos t1 (TMu v ty) = do
  let unrolled = applySubst (Map.singleton v (TMu v ty)) ty
  unifyTypes pos t1 unrolled
-- Arrow types
unifyInternal pos (TArr a1 b1) (TArr a2 b2) = do
  s1 <- unifyTypes pos a1 a2
  s2 <- unifyTypes pos (applySubst s1 b1) (applySubst s1 b2)
  pure (composeSubst s2 s1)
-- Type application
unifyInternal pos (TApp f1 a1) (TApp f2 a2) = do
  s1 <- unifyTypes pos f1 f2
  s2 <- unifyTypes pos (applySubst s1 a1) (applySubst s1 a2)
  pure (composeSubst s2 s1)
-- Tuples
unifyInternal pos (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = unifyList pos ts1 ts2
  | otherwise = throwError $ UnificationError pos (TTuple ts1) (TTuple ts2) "Tuple lengths differ"
-- Records (row polymorphism)
unifyInternal pos (TRecord fs1 r1) (TRecord fs2 r2) = unifyRecords pos fs1 r1 fs2 r2
-- Variants (row polymorphism)
unifyInternal pos (TVariant cs1 r1) (TVariant cs2 r2) = unifyVariants pos cs1 r1 cs2 r2
-- Forall: instantiate then unify
unifyInternal pos (TForall v ty) t2 = do
  fresh <- freshTyVar
  let body = applySubst (Map.singleton v fresh) ty
  unifyTypes pos body t2
unifyInternal pos t1 (TForall v ty) = do
  fresh <- freshTyVar
  let body = applySubst (Map.singleton v fresh) ty
  unifyTypes pos t1 body
-- Mismatch
unifyInternal pos t1 t2 =
  throwError $ UnificationError pos t1 t2 "Cannot unify types"

-- | Unify a list of types pairwise
unifyList :: (UnifyEffs es, State (Set (Ty, Ty)) :> es) => Range -> [Ty] -> [Ty] -> Eff es Subst
unifyList _ [] [] = pure Map.empty
unifyList pos (t1 : ts1) (t2 : ts2) = do
  s1 <- unifyTypes pos t1 t2
  s2 <- unifyList pos (map (applySubst s1) ts1) (map (applySubst s1) ts2)
  pure (composeSubst s2 s1)
unifyList _ _ _ = pure Map.empty

-- | Record row unification
unifyRecords ::
  (UnifyEffs es, State (Set (Ty, Ty)) :> es) =>
  Range ->
  [(T.Text, Ty)] ->
  Maybe Ty ->
  [(T.Text, Ty)] ->
  Maybe Ty ->
  Eff es Subst
unifyRecords pos fs1 r1 fs2 r2 = do
  let names1 = map fst fs1
      names2 = map fst fs2
      commonNames = filter (`elem` names2) names1
      only1 = filter (\(n, _) -> n `notElem` names2) fs1
      only2 = filter (\(n, _) -> n `notElem` names1) fs2

  -- Unify common fields
  commonSubst <-
    foldlM'
      ( \s name -> do
          let t1 = applySubst s $ lookupField name fs1
              t2 = applySubst s $ lookupField name fs2
          s' <- unifyTypes pos t1 t2
          pure (composeSubst s' s)
      )
      Map.empty
      commonNames

  -- Handle row tails
  case (r1, r2) of
    (Nothing, Nothing) ->
      if null only1 && null only2
        then pure commonSubst
        else throwError $ UnificationError pos (TRecord fs1 r1) (TRecord fs2 r2) "Record field mismatch"
    (Just row1, Nothing) ->
      if null only1
        then do
          let only2' = map (second (applySubst commonSubst)) only2
          s <- unifyTypes pos (applySubst commonSubst row1) (TRecord only2' Nothing)
          pure (composeSubst s commonSubst)
        else throwError $ UnificationError pos (TRecord fs1 r1) (TRecord fs2 r2) "Record field mismatch: left has extra fields"
    (Nothing, Just row2) ->
      if null only2
        then do
          let only1' = map (second (applySubst commonSubst)) only1
          s <- unifyTypes pos (applySubst commonSubst row2) (TRecord only1' Nothing)
          pure (composeSubst s commonSubst)
        else throwError $ UnificationError pos (TRecord fs1 r1) (TRecord fs2 r2) "Record field mismatch: right has extra fields"
    (Just row1, Just row2) -> do
      -- Both have tails: create fresh row variable
      freshRow <- freshTyVar
      let only2' = map (second (applySubst commonSubst)) only2
          only1' = map (second (applySubst commonSubst)) only1
      s1 <- unifyTypes pos (applySubst commonSubst row1) (TRecord only2' (Just freshRow))
      s2 <- unifyTypes pos (applySubst (composeSubst s1 commonSubst) row2) (applySubst s1 (TRecord only1' (Just freshRow)))
      pure (composeSubst s2 (composeSubst s1 commonSubst))

-- | Variant row unification
unifyVariants ::
  (UnifyEffs es, State (Set (Ty, Ty)) :> es) =>
  Range ->
  [(T.Text, [Ty])] ->
  Maybe Ty ->
  [(T.Text, [Ty])] ->
  Maybe Ty ->
  Eff es Subst
unifyVariants pos cs1 r1 cs2 r2 = do
  let names1 = map fst cs1
      names2 = map fst cs2
      commonNames = filter (`elem` names2) names1
      only1 = filter (\(n, _) -> n `notElem` names2) cs1
      only2 = filter (\(n, _) -> n `notElem` names1) cs2

  -- Unify common constructors
  commonSubst <-
    foldlM'
      ( \s name -> do
          let tys1 = lookupCon name cs1
              tys2 = lookupCon name cs2
          if length tys1 /= length tys2
            then throwError $ UnificationError pos (TVariant cs1 r1) (TVariant cs2 r2) $ "Constructor '" <> name <> "' has different arities"
            else do
              s' <- unifyList pos (map (applySubst s) tys1) (map (applySubst s) tys2)
              pure (composeSubst s' s)
      )
      Map.empty
      commonNames

  -- Handle row tails
  case (r1, r2) of
    (Nothing, Nothing) ->
      if null only1 && null only2
        then pure commonSubst
        else throwError $ UnificationError pos (TVariant cs1 r1) (TVariant cs2 r2) "Variant mismatch"
    (Just row1, Nothing) ->
      if null only1
        then do
          s <- unifyTypes pos (applySubst commonSubst row1) (TVariant only2 Nothing)
          pure (composeSubst s commonSubst)
        else throwError $ UnificationError pos (TVariant cs1 r1) (TVariant cs2 r2) "Variant mismatch: left has extra constructors"
    (Nothing, Just row2) ->
      if null only2
        then do
          s <- unifyTypes pos (applySubst commonSubst row2) (TVariant only1 Nothing)
          pure (composeSubst s commonSubst)
        else throwError $ UnificationError pos (TVariant cs1 r1) (TVariant cs2 r2) "Variant mismatch: right has extra constructors"
    (Just row1, Just row2) -> do
      freshRow <- freshTyVar
      s1 <- unifyTypes pos (applySubst commonSubst row1) (TVariant only2 (Just freshRow))
      s2 <- unifyTypes pos (applySubst (composeSubst s1 commonSubst) row2) (applySubst s1 (TVariant only1 (Just freshRow)))
      pure (composeSubst s2 (composeSubst s1 commonSubst))

-- | Solve all constraints
solveConstraints :: (UnifyEffs es) => Eff es Subst
solveConstraints = do
  st <- get
  let cs = reverse st.constraints
      baseSubst = st.solvedSubst
  s <- foldlM' (solveOne baseSubst) Map.empty cs
  commitSubst s
  modify (\st' -> st' {constraints = []})
  currentSubst
  where
    solveOne :: (UnifyEffs es) => Subst -> Subst -> TyConstraint -> Eff es Subst
    solveOne baseSubst acc (CUnify pos t1 t2) = do
      let subst = composeSubst acc baseSubst
      let t1' = applySubst subst t1
          t2' = applySubst subst t2
      s <- evalState (Set.empty @(Ty, Ty)) $ unifyTypes pos t1' t2'
      pure $ composeSubst s acc
    solveOne baseSubst acc (CBottomProp sources target) = do
      let subst = composeSubst acc baseSubst
          sources' = map (applySubst subst) sources
          target' = applySubst subst target
      -- If any source is bottom, target should also be bottom
      if any isBottom sources'
        then case target' of
          TVar _ _ -> do
            s <- evalState (Set.empty @(Ty, Ty)) $ unifyTypes dummyRange target' TBottom
            pure $ composeSubst s acc
          _ -> pure acc
        else pure acc

-- | Commit a successfully-computed substitution to the global inference state.
commitSubst :: (State GenState :> es) => Subst -> Eff es ()
commitSubst s =
  modify (\st -> st {solvedSubst = composeSubst s st.solvedSubst})

-- | Check if a type is bottom (after substitution)
isBottom :: Ty -> Bool
isBottom TBottom = True
isBottom _ = False

-- | Compose two substitutions: s2 after s1
composeSubst :: Subst -> Subst -> Subst
composeSubst s2 s1 = Map.map (applySubst s2) s1 <> s2

-- | Look up a field type in a field list
lookupField :: T.Text -> [(T.Text, Ty)] -> Ty
lookupField name fields = case lookup name fields of
  Just ty -> ty
  Nothing -> error $ "lookupField: field not found: " <> T.unpack name

-- | Look up a constructor's argument types in a case list
lookupCon :: T.Text -> [(T.Text, [Ty])] -> [Ty]
lookupCon name cases = case lookup name cases of
  Just tys -> tys
  Nothing -> error $ "lookupCon: constructor not found: " <> T.unpack name

-- | Strict foldlM to avoid space leaks
foldlM' :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldlM' _ z [] = pure z
foldlM' f z (x : xs) = do
  z' <- f z x
  z' `seq` foldlM' f z' xs
