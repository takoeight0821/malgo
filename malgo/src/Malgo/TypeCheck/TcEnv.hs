{-# LANGUAGE TemplateHaskell #-}

module Malgo.TypeCheck.TcEnv
  ( TcEnv (..),
    varEnv,
    typeEnv,
    abbrEnv,
    fieldEnv,
    rnEnv,
    genTcEnv,
    findBuiltinType,
  )
where

import Data.Foldable (find)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import Koriel.Id
import Koriel.Pretty
import Malgo.Prelude
import Malgo.Rename.RnEnv (RnEnv)
import qualified Malgo.Rename.RnEnv as R
import Malgo.Syntax.Extension
import Malgo.TypeRep.Static (Scheme, TypeDef (..), TypeF)
import qualified Malgo.TypeRep.Static as Static
import Malgo.TypeRep.UTerm
import Malgo.UTerm

data TcEnv = TcEnv
  { _varEnv :: HashMap RnId (Scheme UType),
    _typeEnv :: HashMap RnId (TypeDef UType),
    _abbrEnv :: HashMap (Id UType) ([Id UType], UType),
    _fieldEnv :: HashMap RnId (Scheme UType),
    _rnEnv :: RnEnv
  }
  deriving stock (Show)

makeLenses ''TcEnv

instance Pretty TcEnv where
  pPrint TcEnv {..} =
    "TcEnv"
      <+> braces
        ( sep
            [ "_varEnv" <+> "=" <+> pPrint (HashMap.toList _varEnv),
              "_typeEnv" <+> "=" <+> pPrint (HashMap.toList _typeEnv),
              "_abbrEnv" <+> "=" <+> pPrint (HashMap.toList _abbrEnv),
              "_fieldEnv" <+> "=" <+> pPrint (HashMap.toList _fieldEnv),
              "_rnEnv" <+> "=" <+> pPrint _rnEnv
            ]
        )

instance HasUTerm TypeF TypeVar TcEnv where
  walkOn f TcEnv {..} =
    TcEnv <$> traverseOf (traversed . traversed . walkOn) f _varEnv
      <*> traverseOf (traversed . traversed . walkOn) f _typeEnv
      <*> traverseOf (traversed . traversed . walkOn) f _abbrEnv
      <*> traverseOf (traversed . traversed . walkOn) f _fieldEnv
      <*> pure _rnEnv

genTcEnv :: Applicative f => RnEnv -> f TcEnv
genTcEnv rnEnv = do
  let int32_t = fromJust $ findBuiltinType "Int32#" rnEnv
  let int64_t = fromJust $ findBuiltinType "Int64#" rnEnv
  let float_t = fromJust $ findBuiltinType "Float#" rnEnv
  let double_t = fromJust $ findBuiltinType "Double#" rnEnv
  let char_t = fromJust $ findBuiltinType "Char#" rnEnv
  let string_t = fromJust $ findBuiltinType "String#" rnEnv
  pure $
    TcEnv
      { _varEnv = mempty,
        _typeEnv =
          HashMap.fromList
            [ (int32_t, TypeDef (TyPrim Static.Int32T) [] []),
              (int64_t, TypeDef (TyPrim Static.Int64T) [] []),
              (float_t, TypeDef (TyPrim Static.FloatT) [] []),
              (double_t, TypeDef (TyPrim Static.DoubleT) [] []),
              (char_t, TypeDef (TyPrim Static.CharT) [] []),
              (string_t, TypeDef (TyPrim Static.StringT) [] [])
            ],
        _abbrEnv = mempty,
        _fieldEnv = mempty,
        _rnEnv = rnEnv
      }

findBuiltinType :: String -> RnEnv -> Maybe (Id ())
findBuiltinType x rnEnv = do
  ids <- map _value <$> view (R.typeEnv . at x) rnEnv
  find (view idSort >>> \case WiredIn (ModuleName "Builtin") -> True; _ -> False) ids
