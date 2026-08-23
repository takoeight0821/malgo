import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.SExpr

/-! Port of `src/Malgo/Syntax/Extension.hs`. The Haskell trees-that-grow
type families become `abbrev` type-level functions on `Phase`; they are
reducible, so at a concrete phase every extension field elaborates to its
payload type (`Range`, `Id`, …) and `Empty` statically deletes
constructors after Rename. -/

namespace Malgo.Syntax

inductive Phase where
  | parse
  | rename
  deriving BEq, Repr

/-- Haskell `MalgoId`/`XId`: raw text before Rename, resolved `Id` after. -/
abbrev XId : Phase → Type
  | .parse => String
  | .rename => Id

inductive Visibility where
  | explicit (moduleName : ModuleName)
  | implicit
  deriving BEq, Ord, Repr

/-- Qualified name. -/
structure Qualified (α : Type u) where
  visibility : Visibility
  value : α
  deriving BEq, Ord, Repr

instance [HasRange α] : HasRange (Qualified α) := ⟨fun q => range q.value⟩

instance [ToSExpr α] : ToSExpr (Qualified α) where
  toSExpr
    | { visibility := .implicit, value } => toSExpr value
    | { visibility := .explicit m, value } => .list [toSExpr m, toSExpr value]

inductive Assoc where
  | leftA
  | rightA
  | neutralA
  deriving BEq, Repr

instance : ToSExpr Assoc where
  toSExpr
    | .leftA => .atom (.symbol "left")
    | .rightA => .atom (.symbol "right")
    | .neutralA => .atom (.symbol "neutral")

instance : Pretty Assoc where
  pretty
    | .leftA => "l"
    | .rightA => "r"
    | .neutralA => ""

inductive ImportList where
  | all
  | selected (ids : List String)
  | «as» (moduleName : ModuleName)
  deriving BEq, Repr

instance : ToSExpr ImportList where
  toSExpr
    | .all => .atom (.symbol "all")
    | .selected ids => .list (.atom (.symbol "selected") :: ids.map toSExpr)
    | .«as» m => .list [.atom (.symbol "as"), toSExpr m]

/-! Extension slots. Constant-`Range` slots are plain abbrevs so they
reduce even at an abstract phase; only the genuinely phase-varying slots
match on `Phase`. -/

-- Expr slots
abbrev XVar (_ : Phase) : Type := Range
abbrev XUnboxed (_ : Phase) : Type := Range
abbrev XBoxed : Phase → Type
  | .parse => Range
  | .rename => Empty
abbrev XApply (_ : Phase) : Type := Range
abbrev XOpApp : Phase → Type
  | .parse => Range
  | .rename => Range × Assoc × Int
abbrev XProject (_ : Phase) : Type := Range
abbrev XFn (_ : Phase) : Type := Range
abbrev XTuple (_ : Phase) : Type := Range
abbrev XRecord (_ : Phase) : Type := Range
abbrev XList : Phase → Type
  | .parse => Range
  | .rename => Empty
abbrev XAnn (_ : Phase) : Type := Range
abbrev XSeq (_ : Phase) : Type := Range
abbrev XParens (_ : Phase) : Type := Range
abbrev XCodata (_ : Phase) : Type := Range
abbrev XLabel (_ : Phase) : Type := Range
abbrev XGoto (_ : Phase) : Type := Range

-- Clause slot
abbrev XClause (_ : Phase) : Type := Range

-- Stmt slots
abbrev XLet (_ : Phase) : Type := Range
/-- `let` binding a non-variable pattern; desugared away by Rename. -/
abbrev XLetP : Phase → Type
  | .parse => Range
  | .rename => Empty
abbrev XWith : Phase → Type
  | .parse => Range
  | .rename => Empty
abbrev XNoBind (_ : Phase) : Type := Range

-- Pat slots
abbrev XVarP (_ : Phase) : Type := Range
abbrev XConP (_ : Phase) : Type := Range
abbrev XTupleP (_ : Phase) : Type := Range
abbrev XRecordP (_ : Phase) : Type := Range
abbrev XListP : Phase → Type
  | .parse => Range
  | .rename => Empty
abbrev XUnboxedP (_ : Phase) : Type := Range
abbrev XBoxedP : Phase → Type
  | .parse => Range
  | .rename => Empty

-- CoPat slots
abbrev XHoleP (_ : Phase) : Type := Range
abbrev XApplyP (_ : Phase) : Type := Range
abbrev XProjectP (_ : Phase) : Type := Range

-- Type slots
abbrev XTyApp (_ : Phase) : Type := Range
abbrev XTyVar (_ : Phase) : Type := Range
/-- The parser cannot tell constructors from variables; Rename introduces
`TyCon`, so the slot is uninhabited at parse. -/
abbrev XTyCon : Phase → Type
  | .parse => Empty
  | .rename => Range
abbrev XTyArr (_ : Phase) : Type := Range
abbrev XTyTuple (_ : Phase) : Type := Range
abbrev XTyRecord (_ : Phase) : Type := Range
abbrev XTyBlock : Phase → Type
  | .parse => Range
  | .rename => Empty
abbrev XTyBottom (_ : Phase) : Type := Range
abbrev XTyTilde (_ : Phase) : Type := Range
abbrev XTyVariant (_ : Phase) : Type := Range

-- Decl slots
abbrev XScDef (_ : Phase) : Type := Range
abbrev XScSig (_ : Phase) : Type := Range
abbrev XDataDef (_ : Phase) : Type := Range
abbrev XTypeSynonym (_ : Phase) : Type := Range
abbrev XInfix (_ : Phase) : Type := Range
/-- Rename attaches the resolved foreign primitive name. -/
abbrev XForeign : Phase → Type
  | .parse => Range
  | .rename => Range × String
abbrev XImport (_ : Phase) : Type := Range

end Malgo.Syntax
