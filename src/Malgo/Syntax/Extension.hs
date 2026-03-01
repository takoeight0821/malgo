{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Syntax.Extension
  ( MalgoPhase (..),
    Malgo,
    MalgoId,
    Visibility (..),
    Qualified (..),
    Field (..),
    PsId,
    RnId,
    Unboxed,
    Boxed,
    Assoc (..),
    XId,
    SimpleX,
    XVar,
    XCon,
    XUnboxed,
    XBoxed,
    XApply,
    XOpApp,
    XProject,
    XFn,
    XTuple,
    XRecord,
    XList,
    XRecordAccess,
    XAnn,
    XSeq,
    XParens,
    XCodata,
    XLabel,
    XGoto,
    ForallExpX,
    XClause,
    ForallClauseX,
    XLet,
    XWith,
    XNoBind,
    ForallStmtX,
    XVarP,
    XConP,
    XTupleP,
    XRecordP,
    XListP,
    XUnboxedP,
    XBoxedP,
    ForallPatX,
    XHoleP,
    XApplyP,
    XProjectP,
    ForallCoPatX,
    XTyApp,
    XTyVar,
    XTyCon,
    XTyArr,
    XTyTuple,
    XTyRecord,
    XTyBlock,
    XTyBottom,
    XTyTilde,
    XTyVariant,
    ForallTypeX,
    XScDef,
    XScSig,
    XDataDef,
    XTypeSynonym,
    XInfix,
    XForeign,
    XImport,
    ImportList (..),
    ForallDeclX,
    XModule,
  )
where

import Data.Kind qualified as K
import Data.SCargot.Repr.Basic qualified as S
import Data.Store.TH
import Data.Void
import Malgo.Id
import Malgo.Module
import Malgo.Prelude hiding (All)
import Malgo.SExpr (ToSExpr (..))
import Malgo.SExpr qualified as S

-- | Phase and type instance
data MalgoPhase = Parse | Rename

data Malgo (p :: MalgoPhase)

-- | Id
type family MalgoId (p :: MalgoPhase) where
  MalgoId Parse = Text
  MalgoId Rename = Id

data Visibility
  = Explicit ModuleName -- variable that must be qualified
  | Implicit
  deriving stock (Show, Eq, Ord)

instance Pretty Visibility where pretty = pretty . convertString @_ @Text . show

-- | Qualified name
data Qualified x = Qualified {visibility :: Visibility, value :: x}
  deriving stock (Eq, Ord, Show)

instance (HasRange x) => HasRange (Qualified x) where
  range (Qualified _ v) = range v

instance (ToSExpr x) => ToSExpr (Qualified x) where
  toSExpr (Qualified Implicit v) = toSExpr v
  toSExpr (Qualified (Explicit x) v) = S.L [toSExpr x, toSExpr v]

instance (Pretty x) => Pretty (Qualified x) where
  pretty (Qualified Implicit v) = pretty v
  pretty (Qualified (Explicit x) v) = pretty x <> "." <> pretty v

-- | Type-annotated field
data Field x = Field {typeAnn :: Maybe Text, field :: x}
  deriving stock (Eq, Ord, Show)

instance (Pretty x) => Pretty (Field x) where
  pretty (Field Nothing v) = pretty v
  pretty (Field (Just x) v) = pretty x <> "." <> pretty v

type PsId = XId (Malgo Parse)

type RnId = XId (Malgo Rename)

data Unboxed

data Boxed

data Assoc = LeftA | RightA | NeutralA
  deriving stock (Eq, Show, Generic)

instance ToSExpr Assoc where
  toSExpr LeftA = S.A $ S.Symbol "left"
  toSExpr RightA = S.A $ S.Symbol "right"
  toSExpr NeutralA = S.A $ S.Symbol "neutral"

instance Pretty Assoc where
  pretty LeftA = "l"
  pretty RightA = "r"
  pretty NeutralA = ""

type family XId x where
  XId (Malgo p) = MalgoId p

-- * Expr Extensions

-- * Simple syntax annotation (ranges)

type family SimpleX (x :: MalgoPhase)

type instance SimpleX Parse = Range

type instance SimpleX Rename = Range

-- type family XVar x where
type family XVar x where
  XVar (Malgo Parse) = SimpleX Parse
  XVar (Malgo Rename) = SimpleX Rename

type family XCon x where
  XCon (Malgo x) = SimpleX x

type family XUnboxed x where
  XUnboxed (Malgo x) = SimpleX x

type family XBoxed x where
  XBoxed (Malgo Parse) = SimpleX Parse
  XBoxed (Malgo _) = Void

type family XApply x where
  XApply (Malgo x) = SimpleX x

type family XOpApp x

type instance XOpApp (Malgo Parse) = SimpleX Parse

type instance XOpApp (Malgo Rename) = (SimpleX Parse, (Assoc, Int))

type family XProject x where
  XProject (Malgo x) = SimpleX x

type family XFn x where
  XFn (Malgo x) = SimpleX x

type family XTuple x where
  XTuple (Malgo x) = SimpleX x

type family XRecord x where
  XRecord (Malgo x) = SimpleX x

type family XList x where
  XList (Malgo Parse) = SimpleX Parse
  XList (Malgo _) = Void

type family XRecordAccess x where
  XRecordAccess (Malgo x) = SimpleX x

type family XAnn x where
  XAnn (Malgo Parse) = SimpleX Parse
  XAnn (Malgo Rename) = SimpleX Rename
  XAnn (Malgo _) = Void

type family XSeq x where
  XSeq (Malgo x) = SimpleX x

type family XParens x where
  XParens (Malgo Parse) = SimpleX Parse
  XParens (Malgo Rename) = SimpleX Rename
  XParens (Malgo _) = Void

type family XCodata x where
  XCodata (Malgo x) = SimpleX x

type family XLabel x where
  XLabel (Malgo x) = SimpleX x

type family XGoto x where
  XGoto (Malgo x) = SimpleX x

type ForallExpX (c :: K.Type -> Constraint) x =
  ( c (XVar x),
    c (XCon x),
    c (XUnboxed x),
    c (XBoxed x),
    c (XApply x),
    c (XOpApp x),
    c (XProject x),
    c (XFn x),
    c (XTuple x),
    c (XRecord x),
    c (XList x),
    c (XRecordAccess x),
    c (XAnn x),
    c (XSeq x),
    c (XParens x),
    c (XCodata x),
    c (XLabel x),
    c (XGoto x)
  )

-- * Clause Extensions

type family XClause x where
  XClause (Malgo x) = SimpleX x

type ForallClauseX (c :: K.Type -> Constraint) x = c (XClause x)

-- * Stmt Extensions

type family XLet x where
  XLet (Malgo _) = SimpleX Parse

type family XWith x where
  XWith (Malgo Parse) = SimpleX Parse
  XWith (Malgo _) = Void

type family XNoBind x where
  XNoBind (Malgo _) = SimpleX Parse

type ForallStmtX (c :: K.Type -> Constraint) x = (c (XLet x), c (XWith x), c (XNoBind x))

-- * Pat Extensions

type family XVarP x where
  XVarP (Malgo x) = SimpleX x

type family XConP x where
  XConP (Malgo x) = SimpleX x

type family XTupleP x where
  XTupleP (Malgo x) = SimpleX x

type family XRecordP x where
  XRecordP (Malgo x) = SimpleX x

type family XListP x where
  XListP (Malgo Parse) = SimpleX Parse
  XListP (Malgo _) = Void

type family XUnboxedP x where
  XUnboxedP (Malgo x) = SimpleX x

type family XBoxedP x where
  XBoxedP (Malgo Parse) = SimpleX Parse
  XBoxedP (Malgo _) = Void

type ForallPatX (c :: K.Type -> Constraint) x = (c (XVarP x), c (XConP x), c (XTupleP x), c (XRecordP x), c (XListP x), c (XUnboxedP x), c (XBoxedP x))

-- * CoPat Extensions

type family XHoleP x where
  XHoleP (Malgo x) = SimpleX x

type family XApplyP x where
  XApplyP (Malgo x) = SimpleX x

type family XProjectP x where
  XProjectP (Malgo x) = SimpleX x

type ForallCoPatX (c :: K.Type -> Constraint) x = (c (XHoleP x), c (XApplyP x), c (XProjectP x))

-- * Type Extensions

type family XTyApp x where
  XTyApp (Malgo _) = SimpleX Parse

type family XTyVar x where
  XTyVar (Malgo _) = SimpleX Parse

type family XTyCon x where
  XTyCon (Malgo Parse) = Void
  XTyCon (Malgo _) = SimpleX Parse

type family XTyArr x where
  XTyArr (Malgo _) = SimpleX Parse

type family XTyTuple x where
  XTyTuple (Malgo _) = SimpleX Parse

type family XTyRecord x where
  XTyRecord (Malgo _) = SimpleX Parse

type family XTyBlock x where
  XTyBlock (Malgo Parse) = SimpleX Parse
  XTyBlock (Malgo _) = Void

type family XTyBottom x where
  XTyBottom (Malgo _) = SimpleX Parse

type family XTyTilde x where
  XTyTilde (Malgo _) = SimpleX Parse

type family XTyVariant x where
  XTyVariant (Malgo _) = SimpleX Parse

type ForallTypeX (c :: K.Type -> Constraint) x =
  (c (XTyApp x), c (XTyVar x), c (XTyCon x), c (XTyArr x), c (XTyTuple x), c (XTyRecord x), c (XTyBlock x), c (XTyBottom x), c (XTyTilde x), c (XTyVariant x))

-- * Decl Extensions

type family XScDef x where
  XScDef (Malgo x) = SimpleX x

type family XScSig x where
  XScSig (Malgo _) = SimpleX Parse

type family XDataDef x where
  XDataDef (Malgo _) = SimpleX Parse

type family XTypeSynonym x where
  XTypeSynonym (Malgo _) = SimpleX Parse

type family XInfix x where
  XInfix (Malgo _) = SimpleX Parse

type family XForeign x

type instance XForeign (Malgo Parse) = SimpleX Parse

type instance XForeign (Malgo Rename) = (SimpleX Parse, Text)

type family XImport x where
  XImport (Malgo _) = SimpleX Parse

data ImportList = All | Selected [PsId] | As ModuleName

deriving stock instance Eq ImportList

deriving stock instance Show ImportList

instance ToSExpr ImportList where
  toSExpr All = S.A $ S.Symbol "all"
  toSExpr (Selected ids) = S.L (S.A (S.Symbol "selected") : map toSExpr ids)
  toSExpr (As moduleName) = S.L [S.A (S.Symbol "as"), toSExpr moduleName]

type ForallDeclX (c :: K.Type -> Constraint) x =
  ( c (XScDef x),
    c (XScSig x),
    c (XDataDef x),
    c (XTypeSynonym x),
    c (XInfix x),
    c (XForeign x),
    c (XImport x),
    ForallExpX c x,
    ForallClauseX c x,
    ForallStmtX c x,
    ForallPatX c x,
    ForallCoPatX c x,
    ForallTypeX c x
  )

-- * Module Extensions

type family XModule x

makeStore ''Assoc
