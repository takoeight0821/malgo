{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Grouping where

import Data.Graph
import qualified Data.Set as Set
import Koriel.Prelude
import Language.Griff.Extension
import Language.Griff.Syntax
import Language.Malgo.Id
import Language.Malgo.Pretty
import qualified Text.PrettyPrint.HughesPJClass as P

data BindGroup x = BindGroup
  { _scDefs :: [[ScDef x]],
    _scSigs :: [ScSig x],
    _dataDefs :: [DataDef x],
    _infixs :: [Infix x],
    _forigns :: [Forign x]
  }

type ScDef x = (XScDef x, XId x, [XId x], Exp x)

type ScSig x = (XScSig x, XId x, Type x)

type DataDef x = (XDataDef x, XTId x, [XTId x], [(XId x, [Type x])])

type Infix x = (XInfix x, Assoc, Int, XId x)

type Forign x = (XForign x, XId x, Type x)

makeLenses ''BindGroup

deriving stock instance (ForallDeclX Eq x, Eq (XId x), Eq (XTId x)) => Eq (BindGroup x)

deriving stock instance (ForallDeclX Show x, Show (XId x), Show (XTId x)) => Show (BindGroup x)

instance (Pretty (XId x), Pretty (XTId x)) => Pretty (BindGroup x) where
  pPrint BindGroup {_scDefs, _scSigs, _dataDefs, _infixs, _forigns} =
    P.sep
      ( P.punctuate ";" $
          map prettyDataDef _dataDefs
            <> map prettyInfix _infixs
            <> map prettyForign _forigns
            <> map prettyScSig _scSigs
            <> concatMap (map prettyScDef) _scDefs
      )
    where
      prettyDataDef (_, d, xs, cs) = P.sep ["data" <+> pPrint d <+> P.sep (map pPrint xs) <+> "=", P.nest 2 $ foldl1 (\a b -> P.sep [a, "|" <+> b]) $ map pprConDef cs]
      pprConDef (con, ts) = pPrint con <+> P.sep (map (pPrintPrec P.prettyNormal 12) ts)
      prettyInfix (_, a, o, x) = "infix" <> pPrint a <+> pPrint o <+> pPrint x
      prettyForign (_, x, t) = "forign import" <+> pPrint x <+> "::" <+> pPrint t
      prettyScSig (_, f, t) = pPrint f <+> "::" <+> pPrint t
      prettyScDef (_, f, xs, e) = P.sep [pPrint f <+> P.sep (map pPrint xs) <+> "=", P.nest 2 $ pPrint e]

makeBindGroup :: (Pretty a, Ord (XId x), XId x ~ Id a) => [Decl x] -> BindGroup x
makeBindGroup ds =
  BindGroup
    { _scDefs = splitScDef (makeSCC $ mapMaybe scDef ds) (mapMaybe scDef ds),
      _scSigs = mapMaybe scSig ds,
      _dataDefs = mapMaybe dataDef ds,
      _infixs = mapMaybe infixDef ds,
      _forigns = mapMaybe forign ds
    }
  where
    scDef (ScDef x f ps e) = Just (x, f, ps, e)
    scDef _ = Nothing
    scSig (ScSig x f t) = Just (x, f, t)
    scSig _ = Nothing
    dataDef (DataDef x t ps cons) = Just (x, t, ps, cons)
    dataDef _ = Nothing
    infixDef (Infix x a o n) = Just (x, a, o, n)
    infixDef _ = Nothing
    forign (Forign x n t) = Just (x, n, t)
    forign _ = Nothing
    splitScDef sccs ds = map (mapMaybe (\n -> find (\d -> n == d ^. _2) ds)) sccs

adjacents :: (Pretty a, Ord (XId x), XId x ~ Id a) => (XScDef x, Id a, [Id a], Exp x) -> (Id a, Int, [Int])
adjacents (_, f, ps, e) = (f, f ^. idUniq, map (view idUniq) $ toList $ freevars e Set.\\ Set.insert f (Set.fromList ps))

makeSCC :: (Pretty a, Ord (XId x), XId x ~ Id a) => [(XScDef x, Id a, [Id a], Exp x)] -> [[Id a]]
makeSCC ds = map flattenSCC $ stronglyConnComp adjacents'
  where
    vertices = map (view _2 . adjacents) ds
    adjacents' = map ((\(l, v, vs) -> (l, v, filter (`elem` vertices) vs)) . adjacents) ds