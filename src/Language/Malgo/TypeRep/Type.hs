{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.TypeRep.Type where

import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Text.PrettyPrint.HughesPJClass ( parens
                                                , sep
                                                , punctuate
                                                , braces
                                                , text
                                                )

type TyVar = Int

data Scheme = Forall [TyVar] Type
  deriving stock (Eq, Show, Ord)

-- | Malgoの組み込みデータ型
data Type = TyApp TyCon [Type]
          | TyMeta TyVar
  deriving stock (Eq, Show, Ord)

data TyCon = FunC | IntC | FloatC | BoolC | CharC | StringC | TupleC | ArrayC | VariantC [String]
  deriving stock (Eq, Show, Ord)

instance Pretty Scheme where
  pPrint (Forall ts t) = "forall" <+> sep (map pPrint ts) <> "." <+> pPrint t

instance Pretty Type where
  pPrint (TyApp FunC (ret : params)) =
    parens (sep $ punctuate "," $ map pPrint params) <> "->" <> pPrint ret
  pPrint (TyApp TupleC ts) = braces $ sep $ punctuate "," $ map pPrint ts
  pPrint (TyApp (VariantC ls) xs) =
    "<" <> sep (punctuate "," $ zipWith (\l x -> text l <> ":" <+> pPrint x) ls xs) <> ">"
  pPrint (TyApp c ts) = pPrint c <> parens (sep $ punctuate "," $ map pPrint ts)
  pPrint (TyMeta v  ) = pPrint v

instance Pretty TyCon where
  pPrint FunC       = "Fun"
  pPrint IntC       = "Int"
  pPrint FloatC     = "Float"
  pPrint BoolC      = "Bool"
  pPrint CharC      = "Char"
  pPrint StringC    = "String"
  pPrint TupleC     = "Tuple"
  pPrint ArrayC     = "Array"
  pPrint VariantC{} = "Variant"

class HasType a where
  typeOf :: a -> Type

instance HasType Type where
  typeOf = id

comparable :: Type -> Bool
comparable (TyApp IntC   []) = True
comparable (TyApp FloatC []) = True
comparable (TyApp BoolC  []) = True
comparable (TyApp CharC  []) = True
comparable _                 = False

removeExplictForall :: Scheme -> Type
removeExplictForall (Forall _ t) = t

intTy :: Type
intTy = TyApp IntC []
floatTy :: Type
floatTy = TyApp FloatC []
boolTy :: Type
boolTy = TyApp BoolC []
charTy :: Type
charTy = TyApp CharC []
stringTy :: Type
stringTy = TyApp StringC []
tupleTy :: [Type] -> Type
tupleTy = TyApp TupleC
arrayTy :: Type -> Type
arrayTy x = TyApp ArrayC [x]

infixl 6 -->
(-->) :: [Type] -> Type -> Type
ps --> r = TyApp FunC (r : ps)
