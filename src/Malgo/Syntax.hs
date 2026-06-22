{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Syntax
  ( Literal (..),
    Type (..),
    Expr (..),
    Stmt (..),
    Clause (..),
    Pat (..),
    CoPat (..),
    CoClause,
    Decl (..),
    ParsedDefinitions (..),
    BindGroup (..),
    ScDef,
    ScSig,
    DataDef,
    TypeSynonym,
    Foreign,
    Import,
    scDefs,
    scSigs,
    dataDefs,
    typeSynonyms,
    foreigns,
    imports,
    Module (..),
    toUnboxed,
    makeBindGroup,
    freevars,
    patBoundVars,
  )
where

import Data.Graph (flattenSCC, stronglyConnComp)
import Data.List.NonEmpty qualified as NE
import Data.SCargot.Repr.Basic qualified as S
import Data.Set qualified as Set
import Lens.Micro ((^.), _2)
import Lens.Micro.TH (makeLenses)
import Malgo.Module
import Malgo.Prelude hiding (All)
import Malgo.SExpr (ToSExpr (..))
import Malgo.SExpr qualified as S
import Malgo.Syntax.Extension
import Prettyprinter (dquotes, parens, sep, squotes)

sexpr :: [Doc ann] -> Doc ann
sexpr = parens . sep

-- | Unboxed and boxed literal
data Literal x = Int32 Int32 | Int64 Int64 | Float Float | Double Double | Char Char | String Text
  deriving stock (Show, Eq, Ord)

instance ToSExpr (Literal x) where
  toSExpr (Int32 i) = S.L ["int32", S.A $ S.Int (fromIntegral i) Nothing]
  toSExpr (Int64 i) = S.L ["int64", S.A $ S.Int (fromIntegral i) Nothing]
  toSExpr (Float f) = S.L ["float", S.A $ S.Float f]
  toSExpr (Double d) = S.L ["double", S.A $ S.Double d]
  toSExpr (Char c) = S.L ["char", S.A $ S.Char c]
  toSExpr (String s) = S.L ["string", S.A $ S.String s]

instance Pretty (Literal x) where
  pretty (Int32 i) = sexpr ["int32", pretty (toInteger i)]
  pretty (Int64 i) = sexpr ["int64", pretty (toInteger i)]
  pretty (Float f) = sexpr ["float", pretty f]
  pretty (Double d) = sexpr ["double", pretty d]
  pretty (Char c) = sexpr ["char", squotes (pretty c)]
  pretty (String s) = sexpr ["string", dquotes (pretty s)]

toUnboxed :: Literal Boxed -> Literal Unboxed
toUnboxed = coerce

-- * Copattern

data CoPat x
  = HoleP (XHoleP x)
  | ApplyP (XApplyP x) (CoPat x) (Pat x)
  | ProjectP (XProjectP x) (CoPat x) Text

deriving stock instance (ForallCoPatX Eq x, ForallPatX Eq x, Eq (XId x)) => Eq (CoPat x)

deriving stock instance (ForallCoPatX Show x, ForallPatX Show x, Show (XId x)) => Show (CoPat x)

instance (ToSExpr (XId x)) => ToSExpr (CoPat x) where
  toSExpr (HoleP _) = S.A $ S.Symbol "#"
  toSExpr (ApplyP _ cp p) = S.L ["apply", toSExpr cp, toSExpr p]
  toSExpr (ProjectP _ cp field) = S.L ["project", toSExpr cp, S.A $ S.String field]

instance (Pretty (XId x)) => Pretty (CoPat x) where
  pretty (HoleP _) = "#"
  pretty (ApplyP _ cp p) = sexpr ["apply", pretty cp, pretty p]
  pretty (ProjectP _ cp field) = sexpr ["project", pretty cp, pretty field]

instance (ForallCoPatX HasRange x) => HasRange (CoPat x) where
  range (HoleP x) = range x
  range (ApplyP x _ _) = range x
  range (ProjectP x _ _) = range x

-- * Type

data Type x
  = TyApp (XTyApp x) (Type x) [Type x]
  | TyVar (XTyVar x) (XId x)
  | TyCon (XTyCon x) (XId x)
  | TyArr (XTyArr x) (Type x) (Type x)
  | TyTuple (XTyTuple x) [Type x]
  | TyRecord (XTyRecord x) [(Text, Type x)] (Maybe (Type x))
  | TyBlock (XTyBlock x) (Type x)
  | TyBottom (XTyBottom x)
  | TyTilde (XTyTilde x) (Type x)
  | TyVariant (XTyVariant x) [(Text, [Type x])] (Maybe (Type x))

deriving stock instance (ForallTypeX Eq x, Eq (XId x)) => Eq (Type x)

deriving stock instance (ForallTypeX Show x, Show (XId x)) => Show (Type x)

instance (ToSExpr (XId x)) => ToSExpr (Type x) where
  toSExpr (TyApp _ t ts) = S.L ["app", toSExpr t, S.L $ map toSExpr ts]
  toSExpr (TyVar _ v) = toSExpr v
  toSExpr (TyCon _ c) = toSExpr c
  toSExpr (TyArr _ t1 t2) = S.L ["->", toSExpr t1, toSExpr t2]
  toSExpr (TyTuple _ ts) = S.L $ "tuple" : map toSExpr ts
  toSExpr (TyRecord _ kvs rowTail) =
    S.L $ "record" : map (\(k, v) -> S.L [toSExpr k, toSExpr v]) kvs <> maybe [] (\r -> [S.L ["row", toSExpr r]]) rowTail
  toSExpr (TyBlock _ t) = S.L ["block", toSExpr t]
  toSExpr (TyBottom _) = S.A $ S.Symbol "_|_"
  toSExpr (TyTilde _ t) = S.L ["~", toSExpr t]
  toSExpr (TyVariant _ cases rowTail) =
    S.L $ "variant" : map (\(k, ts) -> S.L (toSExpr k : map toSExpr ts)) cases <> maybe [] (\r -> [S.L ["row", toSExpr r]]) rowTail

instance (Pretty (XId x)) => Pretty (Type x) where
  pretty (TyApp _ t ts) = sexpr $ ["app", pretty t] <> map pretty ts
  pretty (TyVar _ v) = pretty v
  pretty (TyCon _ c) = pretty c
  pretty (TyArr _ t1 t2) = sexpr ["->", pretty t1, pretty t2]
  pretty (TyTuple _ ts) = sexpr $ "tuple" : map pretty ts
  pretty (TyRecord _ kvs rowTail) = sexpr $ "record" : map (\(k, v) -> sexpr [pretty k, pretty v]) kvs <> maybe [] (\r -> [sexpr ["row", pretty r]]) rowTail
  pretty (TyBlock _ t) = sexpr ["block", pretty t]
  pretty (TyBottom _) = "_|_"
  pretty (TyTilde _ t) = sexpr ["~", pretty t]
  pretty (TyVariant _ cases rowTail) = sexpr $ "variant" : map (\(k, ts) -> sexpr (pretty k : map pretty ts)) cases <> maybe [] (\r -> [sexpr ["row", pretty r]]) rowTail

-- * Expression

data Expr x
  = Var (XVar x) (XId x)
  | Unboxed (XUnboxed x) (Literal Unboxed)
  | Boxed (XBoxed x) (Literal Boxed)
  | Apply (XApply x) (Expr x) (Expr x)
  | OpApp (XOpApp x) (XId x) (Expr x) (Expr x)
  | Project (XProject x) (Expr x) Text
  | Fn (XFn x) (NonEmpty (Clause x))
  | Tuple (XTuple x) [Expr x]
  | Record (XRecord x) [(Text, Expr x)]
  | List (XList x) [Expr x]
  | Ann (XAnn x) (Expr x) (Type x)
  | Seq (XSeq x) (NonEmpty (Stmt x))
  | Parens (XParens x) (Expr x)
  | Codata (XCodata x) [(CoPat x, Expr x)]
  | Label (XLabel x) (XId x) (Expr x)
  | Goto (XGoto x) (Expr x) (Expr x)

deriving stock instance (ForallExpX Eq x, ForallClauseX Eq x, ForallPatX Eq x, ForallCoPatX Eq x, ForallStmtX Eq x, ForallTypeX Eq x, Eq (XId x)) => Eq (Expr x)

deriving stock instance (ForallExpX Show x, ForallClauseX Show x, ForallPatX Show x, ForallCoPatX Show x, ForallStmtX Show x, ForallTypeX Show x, Show (XId x)) => Show (Expr x)

instance (ToSExpr (XId x)) => ToSExpr (Expr x) where
  toSExpr (Var _ id) = toSExpr id
  toSExpr (Unboxed _ l) = toSExpr l
  toSExpr (Boxed _ l) = toSExpr l
  toSExpr (Apply _ e1 e2) = S.L ["apply", toSExpr e1, toSExpr e2]
  toSExpr (OpApp _ op e1 e2) = S.L ["opapp", toSExpr op, toSExpr e1, toSExpr e2]
  toSExpr (Project _ e k) = S.L ["project", toSExpr e, S.A $ S.String k]
  toSExpr (Fn _ cs) = S.L ["fn", S.L $ map toSExpr $ NE.toList cs]
  toSExpr (Tuple _ es) = S.L $ "tuple" : map toSExpr es
  toSExpr (Record _ kvs) = S.L $ "record" : map (\(k, v) -> S.L [toSExpr k, toSExpr v]) kvs
  toSExpr (List _ es) = S.L $ "list" : map toSExpr es
  toSExpr (Ann _ e t) = S.L ["ann", toSExpr e, toSExpr t]
  toSExpr (Seq _ ss) = S.L $ "seq" : map toSExpr (NE.toList ss)
  toSExpr (Parens _ e) = S.L ["parens", toSExpr e]
  toSExpr (Codata _ clauses) = S.L $ "codata" : map (\(cp, e) -> S.L [toSExpr cp, toSExpr e]) clauses
  toSExpr (Label _ name body) = S.L ["label", toSExpr name, toSExpr body]
  toSExpr (Goto _ value label) = S.L ["goto", toSExpr value, toSExpr label]

instance (Pretty (XId x)) => Pretty (Expr x) where
  pretty (Var _ id) = pretty id
  pretty (Unboxed _ l) = sexpr ["unboxed", pretty l]
  pretty (Boxed _ l) = sexpr ["boxed", pretty l]
  pretty (Apply _ e1 e2) = sexpr ["apply", pretty e1, pretty e2]
  pretty (OpApp _ op e1 e2) = sexpr ["opapp", pretty op, pretty e1, pretty e2]
  pretty (Project _ e k) = sexpr ["project", pretty e, pretty k]
  pretty (Fn _ cs) = sexpr ["fn", sexpr $ map pretty (toList cs)]
  pretty (Tuple _ es) = sexpr $ "tuple" : map pretty es
  pretty (Record _ kvs) = sexpr $ "record" : map (\(k, v) -> sexpr [pretty k, pretty v]) kvs
  pretty (List _ es) = sexpr $ "list" : map pretty es
  pretty (Ann _ e t) = sexpr ["ann", pretty e, pretty t]
  pretty (Seq _ ss) = sexpr $ "seq" : map pretty (toList ss)
  pretty (Parens _ e) = sexpr ["parens", pretty e]
  pretty (Codata _ clauses) = sexpr $ "codata" : map (\(cp, e) -> sexpr [pretty cp, pretty e]) clauses
  pretty (Label _ name body) = sexpr ["label", pretty name, pretty body]
  pretty (Goto _ value label) = sexpr ["goto", pretty value, pretty label]

instance (ForallExpX HasRange x) => HasRange (Expr x) where
  range (Var x _) = range x
  range (Unboxed x _) = range x
  range (Boxed x _) = range x
  range (Apply x _ _) = range x
  range (OpApp x _ _ _) = range x
  range (Project x _ _) = range x
  range (Fn x _) = range x
  range (Tuple x _) = range x
  range (Record x _) = range x
  range (List x _) = range x
  range (Ann x _ _) = range x
  range (Seq x _) = range x
  range (Parens x _) = range x
  range (Codata x _) = range x
  range (Label x _ _) = range x
  range (Goto x _ _) = range x

-- | Variables bound by a pattern.
patBoundVars :: (Ord (XId x)) => Pat x -> Set (XId x)
patBoundVars (VarP _ x) = Set.singleton x
patBoundVars (ConP _ _ ps) = foldMap patBoundVars ps
patBoundVars (TupleP _ ps) = foldMap patBoundVars ps
patBoundVars (RecordP _ kps) = foldMap (patBoundVars . snd) kps
patBoundVars (ListP _ ps) = foldMap patBoundVars ps
patBoundVars UnboxedP {} = mempty
patBoundVars BoxedP {} = mempty

freevars :: (Ord (XId x)) => Expr x -> Set (XId x)
freevars (Var _ v) = Set.singleton v
freevars (Unboxed _ _) = mempty
freevars (Boxed _ _) = mempty
freevars (Apply _ e1 e2) = freevars e1 <> freevars e2
freevars (OpApp _ op e1 e2) = Set.insert op $ freevars e1 <> freevars e2
freevars (Project _ e _) = freevars e
freevars (Fn _ cs) = foldMap freevarsClause cs
  where
    freevarsClause :: (Ord (XId x)) => Clause x -> Set (XId x)
    freevarsClause (Clause _ pats e) = Set.difference (freevars e) (foldMap patBoundVars pats)
freevars (Tuple _ es) = mconcat $ map freevars es
freevars (Record _ kvs) = mconcat $ map (freevars . snd) kvs
freevars (List _ es) = mconcat $ map freevars es
freevars (Ann _ e _) = freevars e
freevars (Seq _ ss) = freevarsStmts ss
  where
    freevarsStmts (Let _ x e :| ss) = freevars e <> Set.delete x (freevarsStmts' ss)
    freevarsStmts (LetP _ pat e :| ss) = freevars e <> Set.difference (freevarsStmts' ss) (patBoundVars pat)
    freevarsStmts (With _ Nothing e :| ss) = freevars e <> freevarsStmts' ss
    freevarsStmts (With _ (Just x) e :| ss) = freevars e <> Set.delete x (freevarsStmts' ss)
    freevarsStmts (NoBind _ e :| ss) = freevars e <> freevarsStmts' ss
    freevarsStmts' [] = mempty
    freevarsStmts' (s : ss) = freevarsStmts (s :| ss)
freevars (Parens _ e) = freevars e
freevars (Codata _ clauses) = foldMap freevarsClause clauses
  where
    freevarsClause :: (Ord (XId x)) => (CoPat x, Expr x) -> Set (XId x)
    freevarsClause (copat, e) = freevars e Set.\\ copatBoundVars copat
    copatBoundVars :: (Ord (XId x)) => CoPat x -> Set (XId x)
    copatBoundVars (HoleP _) = mempty
    copatBoundVars (ApplyP _ cp pat) = copatBoundVars cp <> patBoundVars pat
    copatBoundVars (ProjectP _ cp _) = copatBoundVars cp
freevars (Label _ name body) = Set.delete name (freevars body)
freevars (Goto _ value label) = freevars value <> freevars label

-- * Stmt

data Stmt x
  = Let (XLet x) (XId x) (Expr x)
  | -- | @let pat = e@ where @pat@ is not a bare variable. Desugared to a
    -- @case@ in 'Malgo.Rename.Pass'; uninhabited after the parse phase.
    LetP (XLetP x) (Pat x) (Expr x)
  | With (XWith x) (Maybe (XId x)) (Expr x)
  | NoBind (XNoBind x) (Expr x)

deriving stock instance (ForallClauseX Eq x, ForallPatX Eq x, ForallCoPatX Eq x, ForallExpX Eq x, ForallStmtX Eq x, ForallTypeX Eq x, Eq (XId x)) => Eq (Stmt x)

deriving stock instance (ForallClauseX Show x, ForallPatX Show x, ForallCoPatX Show x, ForallExpX Show x, ForallStmtX Show x, ForallTypeX Show x, Show (XId x)) => Show (Stmt x)

instance (ToSExpr (XId x)) => (ToSExpr (Stmt x)) where
  toSExpr (Let _ id e) = S.L ["let", toSExpr id, toSExpr e]
  toSExpr (LetP _ pat e) = S.L ["let", toSExpr pat, toSExpr e]
  toSExpr (With _ Nothing e) = S.L ["with", toSExpr e]
  toSExpr (With _ (Just id) e) = S.L ["with", toSExpr id, toSExpr e]
  toSExpr (NoBind _ e) = S.L ["do", toSExpr e]

instance (Pretty (XId x)) => Pretty (Stmt x) where
  pretty (Let _ var body) = sexpr ["let", pretty var, pretty body]
  pretty (LetP _ pat body) = sexpr ["let", pretty pat, pretty body]
  pretty (With _ Nothing body) = sexpr ["with", pretty body]
  pretty (With _ (Just var) body) = sexpr ["with", pretty var, pretty body]
  pretty (NoBind _ body) = sexpr ["do", pretty body]

-- * CoClause

type CoClause x = (CoPat x, Expr x)

-- * Clause

data Clause x = Clause (XClause x) (NonEmpty (Pat x)) (Expr x)

deriving stock instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x, ForallCoPatX Eq x, ForallStmtX Eq x, ForallTypeX Eq x, Eq (XId x)) => Eq (Clause x)

deriving stock instance (ForallClauseX Show x, ForallExpX Show x, ForallPatX Show x, ForallCoPatX Show x, ForallStmtX Show x, ForallTypeX Show x, Show (XId x)) => Show (Clause x)

instance (ForallClauseX Eq x, ForallExpX Eq x, ForallPatX Eq x, ForallCoPatX Eq x, Ord (XId x), ForallPatX Ord x, ForallCoPatX Ord x, ForallStmtX Ord x, ForallTypeX Ord x) => Ord (Clause x) where
  (Clause _ ps1 _) `compare` (Clause _ ps2 _) = ps1 `compare` ps2

instance (ToSExpr (XId x)) => ToSExpr (Clause x) where
  toSExpr (Clause _ pats body) = S.L ["clause", S.L $ toList $ fmap toSExpr pats, toSExpr body]

instance (Pretty (XId x)) => Pretty (Clause x) where
  pretty (Clause _ pats body) = sexpr ["clause", sexpr $ toList $ fmap pretty pats, pretty body]

-- * Pattern

data Pat x
  = VarP (XVarP x) (XId x)
  | ConP (XConP x) (XId x) [Pat x]
  | TupleP (XTupleP x) [Pat x]
  | RecordP (XRecordP x) [(Text, Pat x)]
  | ListP (XListP x) [Pat x]
  | UnboxedP (XUnboxedP x) (Literal Unboxed)
  | BoxedP (XBoxedP x) (Literal Boxed)

deriving stock instance (ForallPatX Eq x, Eq (XId x)) => Eq (Pat x)

deriving stock instance (ForallPatX Show x, Show (XId x)) => Show (Pat x)

deriving stock instance (ForallPatX Ord x, Ord (XId x)) => Ord (Pat x)

instance (ToSExpr (XId x)) => ToSExpr (Pat x) where
  toSExpr (VarP _ id) = toSExpr id
  toSExpr (ConP _ id ps) = S.L ["con", toSExpr id, S.L $ map toSExpr ps]
  toSExpr (TupleP _ ps) = S.L $ "tuple" : map toSExpr ps
  toSExpr (RecordP _ kps) = S.L $ "record" : map (\(k, p) -> S.L [toSExpr k, toSExpr p]) kps
  toSExpr (ListP _ ps) = S.L $ "list" : map toSExpr ps
  toSExpr (UnboxedP _ l) = S.L ["unboxed", toSExpr l]
  toSExpr (BoxedP _ l) = S.L ["boxed", toSExpr l]

instance (Pretty (XId x)) => Pretty (Pat x) where
  pretty (VarP _ id) = pretty id
  pretty (ConP _ id ps) = sexpr $ ["con", pretty id] <> map pretty ps
  pretty (TupleP _ ps) = sexpr $ "tuple" : map pretty ps
  pretty (RecordP _ kps) = sexpr $ "record" : map (\(k, p) -> sexpr [pretty k, pretty p]) kps
  pretty (ListP _ ps) = sexpr $ "list" : map pretty ps
  pretty (UnboxedP _ l) = sexpr ["unboxed", pretty l]
  pretty (BoxedP _ l) = sexpr ["boxed", pretty l]

-- * Declaration

data Decl x
  = ScDef (XScDef x) (XId x) (Expr x)
  | ScSig (XScSig x) (XId x) (Type x)
  | DataDef (XDataDef x) (XId x) [(Range, XId x)] [(Range, XId x, [Type x])]
  | TypeSynonym (XTypeSynonym x) (XId x) [XId x] (Type x)
  | Infix (XInfix x) Assoc Int (XId x)
  | Foreign (XForeign x) (XId x) (Type x)
  | Import (XImport x) ModuleName ImportList

deriving stock instance (ForallDeclX Eq x, Eq (XId x)) => Eq (Decl x)

deriving stock instance (ForallDeclX Show x, Show (XId x)) => Show (Decl x)

instance (ToSExpr (XId x)) => ToSExpr (Decl x) where
  toSExpr (ScDef _ f e) = S.L ["def", toSExpr f, toSExpr e]
  toSExpr (ScSig _ f t) = S.L ["sig", toSExpr f, toSExpr t]
  toSExpr (DataDef _ t ps cons) =
    S.L
      [ "data",
        toSExpr t,
        S.L $ map (toSExpr . view _2) ps,
        S.L $ map (\(_, c, ts) -> S.L [toSExpr c, S.L $ map toSExpr ts]) cons
      ]
  toSExpr (TypeSynonym _ t ps ty) = S.L ["type", toSExpr t, S.L $ map toSExpr ps, toSExpr ty]
  toSExpr (Infix _ assoc prec op) = S.L ["infix", toSExpr assoc, S.A $ S.Int (fromIntegral prec) Nothing, toSExpr op]
  toSExpr (Foreign _ n t) = S.L ["foreign", toSExpr n, toSExpr t]
  toSExpr (Import _ m list) = S.L ["import", toSExpr m, toImportList list]
    where
      toImportList All = "all"
      toImportList (Selected xs) = S.L $ "selected" : map toSExpr xs
      toImportList (As m) = S.L ["as", toSExpr m]

instance (Pretty (XId x)) => Pretty (Decl x) where
  pretty (ScDef x name expr) = prettyScDef (x, name, expr)
  pretty (ScSig x name ty) = prettyScSig (x, name, ty)
  pretty (DataDef x name params cons) = prettyDataDef (x, name, params, cons)
  pretty (TypeSynonym x name params ty) = prettyTypeSynonym (x, name, params, ty)
  pretty (Infix _ assoc prec op) = sexpr ["infix", pretty assoc, pretty prec, pretty op]
  pretty (Foreign x name ty) = prettyForeign (x, name, ty)
  pretty (Import x name list) = prettyImport (x, name, list)

prettyScDef :: (Pretty (XId x)) => ScDef x -> Doc ann
prettyScDef (_, f, e) = sexpr ["def", pretty f, pretty e]

prettyScSig :: (Pretty (XId x)) => ScSig x -> Doc ann
prettyScSig (_, f, t) = sexpr ["sig", pretty f, pretty t]

prettyDataDef :: (Pretty (XId x)) => DataDef x -> Doc ann
prettyDataDef (_, t, ps, cons) =
  sexpr
    [ "data",
      pretty t,
      sexpr $ map (pretty . view _2) ps,
      sexpr $ map prettyConDef cons
    ]
  where
    prettyConDef (_, con, ts) = sexpr [pretty con, sexpr $ map pretty ts]

prettyTypeSynonym :: (Pretty (XId x)) => TypeSynonym x -> Doc ann
prettyTypeSynonym (_, t, ps, ty) = sexpr ["type", pretty t, sexpr $ map pretty ps, pretty ty]

prettyForeign :: (Pretty (XId x)) => Foreign x -> Doc ann
prettyForeign (_, n, t) = sexpr ["foreign", pretty n, pretty t]

-- prettyImport :: Import x -> Doc ann
prettyImport :: (x, ModuleName, ImportList) -> Doc ann
prettyImport (_, m, list) =
  sexpr ["import", pretty m, prettyImportList list]
  where
    prettyImportList All = "all"
    prettyImportList (Selected xs) = sexpr $ "selected" : map pretty xs
    prettyImportList (As m) = sexpr ["as", pretty m]

-- * Module

data Module x = Module {moduleName :: ModuleName, moduleDefinition :: XModule x}

deriving stock instance (ForallDeclX Eq x, Eq (XId x), Eq (XModule x)) => Eq (Module x)

deriving stock instance (ForallDeclX Show x, Show (XId x), Show (XModule x)) => Show (Module x)

instance (ToSExpr (XId x), ToSExpr (XModule x)) => ToSExpr (Module x) where
  toSExpr (Module name defs) =
    S.L ["module", toSExpr name, toSExpr defs]

instance (Pretty (XId x), Pretty (XModule x)) => Pretty (Module x) where
  pretty (Module name defs) =
    sexpr ["module", pretty name, pretty defs]

newtype ParsedDefinitions x = ParsedDefinitions [Decl x]

deriving stock instance (ForallDeclX Eq x, Eq (XId x)) => Eq (ParsedDefinitions x)

deriving stock instance (ForallDeclX Show x, Show (XId x)) => Show (ParsedDefinitions x)

instance (ToSExpr (XId x)) => ToSExpr (ParsedDefinitions x) where
  toSExpr (ParsedDefinitions ds) = S.L $ map toSExpr ds

instance (ForallDeclX Pretty x, Pretty (XId x)) => Pretty (ParsedDefinitions x) where
  pretty (ParsedDefinitions ds) = sep $ map pretty ds

-- モジュールの循環参照を防ぐため、このモジュールでtype instanceを定義する
type instance XModule (Malgo Parse) = ParsedDefinitions (Malgo Parse)

type instance XModule (Malgo Rename) = BindGroup (Malgo Rename)

-- * Bind group

data BindGroup x = BindGroup
  { -- | 相互再帰的なグループに分割
    _scDefs :: [[ScDef x]],
    _scSigs :: [ScSig x],
    _dataDefs :: [DataDef x],
    _typeSynonyms :: [TypeSynonym x],
    _foreigns :: [Foreign x],
    _imports :: [Import x]
  }

type ScDef x = (XScDef x, XId x, Expr x)

type ScSig x = (XScSig x, XId x, Type x)

type DataDef x = (XDataDef x, XId x, [(Range, XId x)], [(Range, XId x, [Type x])])

type TypeSynonym x = (XTypeSynonym x, XId x, [XId x], Type x)

type Foreign x = (XForeign x, XId x, Type x)

type Import x = (XImport x, ModuleName, ImportList)

makeLenses ''BindGroup

deriving stock instance (ForallDeclX Eq x, Eq (XId x)) => Eq (BindGroup x)

deriving stock instance (ForallDeclX Show x, Show (XId x)) => Show (BindGroup x)

instance (ToSExpr (XId x)) => ToSExpr (BindGroup x) where
  toSExpr BindGroup {..} =
    S.L
      [ S.L $ map (S.L . map toSExprScDef) _scDefs,
        S.L $ map toSExprScSig _scSigs,
        S.L $ map toSExprDataDef _dataDefs,
        S.L $ map toSExprTypeSynonym _typeSynonyms,
        S.L $ map toSExprForeign _foreigns,
        S.L $ map toSExprImport _imports
      ]
    where
      toSExprScDef (_, f, e) = S.L ["def", toSExpr f, toSExpr e]
      toSExprScSig (_, f, t) = S.L ["sig", toSExpr f, toSExpr t]
      toSExprDataDef (_, name, ps, cons) =
        S.L
          [ "data",
            toSExpr name,
            S.L $ map (toSExpr . view _2) ps,
            S.L $ map (\(_, c, ts) -> S.L [toSExpr c, S.L $ map toSExpr ts]) cons
          ]
      toSExprTypeSynonym (_, name, ps, ty) =
        S.L ["type", toSExpr name, S.L $ map toSExpr ps, toSExpr ty]
      toSExprForeign (_, n, t) = S.L ["foreign", toSExpr n, toSExpr t]
      toSExprImport (_, m, list) =
        S.L ["import", toSExpr m, toImportList list]
        where
          toImportList All = "all"
          toImportList (Selected xs) = S.L $ "selected" : map toSExpr xs
          toImportList (As m) = S.L ["as", toSExpr m]

instance (Pretty (XId x)) => Pretty (BindGroup x) where
  pretty BindGroup {..} =
    sep
      [ sexpr $ map prettyScDefs _scDefs,
        sexpr $ map prettyScSig _scSigs,
        sexpr $ map prettyDataDef _dataDefs,
        sexpr $ map prettyTypeSynonym _typeSynonyms,
        sexpr $ map prettyForeign _foreigns,
        sexpr $ map prettyImport _imports
      ]
    where
      prettyScDefs = sexpr . map prettyScDef

makeBindGroup :: (Ord (XId x)) => [Decl x] -> BindGroup x
makeBindGroup ds =
  BindGroup
    { _scDefs = splitScDef (makeSCC $ mapMaybe scDef ds) (mapMaybe scDef ds),
      _scSigs = mapMaybe scSig ds,
      _dataDefs = mapMaybe dataDef ds,
      _typeSynonyms = mapMaybe typeSynonym ds,
      _foreigns = mapMaybe foreignDef ds,
      _imports = mapMaybe importDef ds
    }
  where
    scDef (ScDef x f e) = Just (x, f, e)
    scDef _ = Nothing
    scSig (ScSig x f t) = Just (x, f, t)
    scSig _ = Nothing
    dataDef (DataDef x t ps cons) = Just (x, t, ps, cons)
    dataDef _ = Nothing
    typeSynonym (TypeSynonym x t ps t') = Just (x, t, ps, t')
    typeSynonym _ = Nothing
    foreignDef (Foreign x n t) = Just (x, n, t)
    foreignDef _ = Nothing
    importDef (Import x m ns) = Just (x, m, ns)
    importDef _ = Nothing
    splitScDef sccs ds = map (mapMaybe (\n -> find (\d -> n == d ^. _2) ds)) sccs

adjacents :: (Ord (XId x)) => (a, XId x, Expr x) -> (XId x, XId x, [XId x])
adjacents (_, f, e) =
  (f, f, toList $ Set.delete f (freevars e))

makeSCC :: (Ord (XId x)) => [(a, XId x, Expr x)] -> [[XId x]]
makeSCC ds = map flattenSCC $ stronglyConnComp adjacents'
  where
    vertices = map (view _2 . adjacents) ds
    adjacents' = map ((\(l, v, vs) -> (l, v, filter (`elem` vertices) vs)) . adjacents) ds
