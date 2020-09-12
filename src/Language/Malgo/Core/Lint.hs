{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Core.Lint
  ( LintExp,
    lint,
  )
where

import Language.Malgo.IR.Core
import Language.Malgo.IR.Op
import Language.Malgo.Id
import Language.Malgo.Pass
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.CType
import Text.PrettyPrint (render, ($$))

data LintExp

-- Core.Expのtype checkを行うPass

instance Pass LintExp (Exp (Id CType)) (Exp (Id CType)) where
  passName = "Exp Lint"
  trans e = do
    lint e
    pure e

lint :: (Monad m, Pretty a, HasCType a) => Exp (Id a) -> m ()
lint e = runReaderT (lintExp e) []

defined :: (MonadReader [Id a] m, Pretty a) => Id a -> m ()
defined x = do
  env <- ask
  unless (x `elem` env) $ error $ render $ pPrint x <> " is not defined"

match ::
  ( HasCType a,
    HasCType b,
    Monad f,
    Pretty a,
    Pretty b,
    HasCallStack
  ) =>
  a ->
  b ->
  f ()
match (cTypeOf -> ps0 :-> r0) (cTypeOf -> ps1 :-> r1) = do
  zipWithM_ match ps0 ps1
  match r0 r1
match (cTypeOf -> DataT {}) (cTypeOf -> VarT {}) = pure ()
match (cTypeOf -> VarT {}) (cTypeOf -> DataT {}) = pure ()
match (cTypeOf -> VarT {}) (cTypeOf -> VarT {}) = pure ()
match x y
  | cTypeOf x == cTypeOf y = pure ()
  | otherwise =
    errorDoc $
      "type mismatch:"
        $$ (pPrint x <+> ":" <+> pPrint (cTypeOf x))
        $$ (pPrint y <+> ":" <+> pPrint (cTypeOf y))

lintExp ::
  ( MonadReader [Id a] m,
    Pretty a,
    HasCType a
  ) =>
  Exp (Id a) ->
  m ()
lintExp (Atom x) = lintAtom x
lintExp (Call f xs) = do
  lintAtom f
  traverse_ lintAtom xs
  case cTypeOf f of
    ps :-> r -> match f (map cTypeOf xs :-> r) >> zipWithM_ match ps xs
    _ -> errorDoc $ pPrint f <+> "is not callable"
lintExp (CallDirect f xs) = do
  defined f
  traverse_ lintAtom xs
  case cTypeOf f of
    ps :-> r -> match f (map cTypeOf xs :-> r) >> zipWithM_ match ps xs
    _ -> errorDoc $ pPrint f <+> "is not callable"
lintExp (PrimCall _ (ps :-> _) xs) = do
  traverse_ lintAtom xs
  zipWithM_ match ps xs
lintExp PrimCall {} = error "primitive must be a function"
lintExp (BinOp o x y) = do
  lintAtom x
  lintAtom y
  case o of
    Add -> match IntT x >> match IntT y
    Sub -> match IntT x >> match IntT y
    Mul -> match IntT x >> match IntT y
    Div -> match IntT x >> match IntT y
    Mod -> match IntT x >> match IntT y
    FAdd -> match FloatT x >> match FloatT y
    FSub -> match FloatT x >> match FloatT y
    FMul -> match FloatT x >> match FloatT y
    FDiv -> match FloatT x >> match FloatT y
    Eq -> match x y
    Neq -> match x y
    Lt -> match x y
    Le -> match x y
    Gt -> match x y
    Ge -> match x y
    _ -> error "And and Or is not supported"
lintExp (ArrayRead a i) = do
  lintAtom a
  lintAtom i
  case cTypeOf a of
    ArrayT _ -> match IntT i
    _ -> errorDoc $ pPrint a <+> "must be a array"
lintExp (ArrayWrite a i v) = do
  lintAtom a
  lintAtom i
  lintAtom v
  case cTypeOf a of
    ArrayT t -> match IntT i >> match t v
    _ -> errorDoc $ pPrint a <+> "must be a array"
lintExp (Cast _ x) = lintAtom x
lintExp (Let ds e) =
  local (map fst ds <>) $ do
    traverse_ (lintObj . snd) ds
    lintExp e
lintExp (Match e cs) = do
  lintExp e
  traverse_ lintCase cs

lintObj ::
  ( MonadReader [Id a] m,
    Pretty a,
    HasCType a
  ) =>
  Obj (Id a) ->
  m ()
lintObj (Fun params body) =
  local (params <>) $ lintExp body
lintObj (Pack _ _ xs) = traverse_ lintAtom xs
lintObj (Array a n) = lintAtom a >> lintAtom n >> match IntT n

lintCase ::
  ( MonadReader [Id a] m,
    Pretty a,
    HasCType a
  ) =>
  Case (Id a) ->
  m ()
lintCase (Unpack _ vs e) = local (vs <>) $ lintExp e
lintCase (Bind x e) = local (x :) $ lintExp e

lintAtom :: (MonadReader [Id a] m, Pretty a) => Atom (Id a) -> m ()
lintAtom (Var x) = defined x
lintAtom (Unboxed _) = pure ()
