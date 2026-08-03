import Malgo.Doc
import Malgo.Id
import Malgo.Module
import Malgo.Data.ShowFloat
import Malgo.Sequent.Fun
import Malgo.Sequent.Core.Full
import Malgo.Sequent.Core.Flat
import Malgo.Sequent.Core.Join
import Malgo.Backend.Zig.Ir
import Malgo.Syntax
import Malgo.Syntax.Extension

/-! Port of `src/Malgo/Debug/PrettyIR.hs`: renders each stage of the
compilation pipeline in Malgo-ish syntax, for the MET (M-exp-Tracer) debug
tool and its golden tests. Deliberately best-effort, non-round-trippable:
the goal is a human-readable text a developer can diff between passes.

The sequent-calculus IRs (Core.Full/Flat/Join) have no direct Malgo
surface-syntax equivalent, so they use a light ASCII notation:
`producer ~ consumer` for a cut, `.field -> k` for a projection/destructor
continuation, and `{ ... }` for statement bodies.

Every nested body goes through `block`/`blockLines`/`renderArgs` rather
than raw braces/parens, so long definitions wrap onto indented lines
instead of rendering as one huge line — which also keeps line-based
diffing meaningful.

Layout is via `Malgo.Doc` (the ported `prettyprinter`); string literals
that are known newline-free become `atom`, dynamic `Text`/`String`/`Id`-name
values go through `fromString`/`renderName`/`renderId` (matching every
`pretty` call in the Haskell source). -/

namespace Malgo.Debug.PrettyIR

open Malgo
open Malgo.Doc
open Malgo.Sequent
open Malgo.Sequent.Core
open Malgo.Backend.Zig

/-! ## Shared helpers (Name/Tag/Literal are the same types across every IR) -/

/-- Drops the module qualifier: a trace is always a single unlinked module,
so every name shares the same qualifier and repeating it is just noise. -/
def renderName (id : Id) : Doc :=
  match id.sort with
  | .external => fromString id.name
  | .internal u => fromString id.name ++ atom "#" ++ atom (toString u)
  | .temporal u => fromString id.name ++ atom "$" ++ atom (toString u)

/-- Surface-syntax identifiers are `String` at the Parse phase and resolved
`Id`s after Rename. Dispatching through this class means Rename-phase output
uses the same plain `name$uniq`/`name#uniq` style as every other IR
(`renderName`) instead of `Id`'s own `Pretty` instance. -/
class RenderId (α : Type) where
  renderId : α → Doc

export RenderId (renderId)

instance : RenderId String := ⟨fromString⟩
instance : RenderId Id := ⟨renderName⟩

def renderTag : Fun.Tag → Doc
  | .tuple => atom "Tuple"
  | .tag t => fromString t

def renderLit : Fun.Literal → Doc
  | .int32 n => atom (toString n.toInt)
  | .int64 n => atom (toString n.toInt) ++ atom "L"
  | .float f => atom (haskellShowFloat32 f) ++ atom "f"
  | .double d => atom (haskellShowFloat d)
  | .char c => squotes (fromString (String.singleton c))
  | .string s => dquotes (fromString s)

/-- Comma-separated items in parens: one line if it fits, else one item per
line (leading comma), indented. -/
def renderArgs : List Doc → Doc := tupled

/-- Like `renderArgs`, but brace-delimited (records/objects). -/
def braceList (ds : List Doc) : Doc :=
  group (encloseSep (flatAlt (lbrace ++ atom " ") lbrace) (flatAlt (atom " " ++ rbrace) rbrace)
    (atom ", ") ds)

/-- A single nested statement/body in braces, always expanded onto its own
indented line(s) — deliberately unconditional (no `group`), so a block's
formatting is a pure function of its content and never flips between
one-line and expanded depending on how much of the line is already used by
its surroundings. Since a `hardline` can never be flattened away by an
enclosing `group`, wrapping every continuation body in `block` forces every
ancestor construct that contains one to break too. -/
def block (content : Doc) : Doc :=
  atom "{" ++ nest 2 (hardline ++ content) ++ hardline ++ atom "}"

/-- Like `vsep`, but never collapsed by an enclosing `group`. -/
def vsepHard : List Doc → Doc := concatWith (fun a b => a ++ hardline ++ b)

/-- A list of branches/arms in braces, always one per line — collapsing
match arms onto one line hurts readability even when they would fit. -/
def blockLines : List Doc → Doc
  | [] => atom "{}"
  | docs => atom "{" ++ nest 2 (hardline ++ vsepHard docs) ++ hardline ++ atom "}"

/-- `prefix = body`: inline if it fits, else `body` indented on the next line. -/
def hangEq (pre body : Doc) : Doc :=
  group (pre <+> atom "=" ++ nest 2 (line ++ body))

def renderPattern : Fun.Pattern → Doc
  | .pvar _ name => renderName name
  | .pliteral _ lit => renderLit lit
  | .destruct _ tag pats => renderTag tag ++ renderArgs (pats.map renderPattern)
  | .expand _ fields =>
    braceList ((sortAssocAscending fields).attach.map fun ⟨kv, hkv⟩ =>
      fromString kv.1 <+> atom "=" <+> renderPattern kv.2)
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_lt_of_mem (mem_sortAssocAscending hkv)) (by omega)
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

/-- One blank line between each top-level definition. -/
def renderDefs (ds : List Doc) : String :=
  Malgo.Doc.render (concatWith (fun a b => a ++ hardline ++ hardline ++ b) ds)

/-! ## Fun IR -/

mutual

def renderExpr : Fun.Expr → Doc
  | .var _ name => renderName name
  | .literal _ lit => renderLit lit
  | .construct _ tag args => renderTag tag ++ renderArgs (args.map renderExpr)
  | .«let» _ name value body =>
    group (atom "let" <+> renderName name <+> atom "="
      ++ nest 2 (line ++ renderExpr value)
      ++ line ++ atom "in" ++ nest 2 (line ++ renderExpr body))
  | .lambda _ params body =>
    group (atom "\\" ++ hsep (params.map renderName) <+> atom "->" ++ nest 2 (line ++ renderExpr body))
  | .object _ fields =>
    braceList ((sortAssocAscending fields).attach.map fun ⟨kv, hkv⟩ =>
      fromString kv.1 <+> atom "=" <+> renderExpr kv.2)
  | .apply _ callee args => renderExpr callee ++ renderArgs (args.map renderExpr)
  | .project _ callee field => renderExpr callee ++ atom "." ++ fromString field
  | .primitive _ op args => atom "#" ++ fromString op ++ renderArgs (args.map renderExpr)
  | .select _ scrutinee branches =>
    atom "case" <+> renderExpr scrutinee <+> atom "of" <+> blockLines (branches.map renderBranch)
  | .invoke _ name => atom "invoke" <+> renderName name
  | .fix _ name body => hangEq (atom "fix" <+> renderName name) (renderExpr body)
termination_by e => sizeOf e
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_lt_of_mem (mem_sortAssocAscending hkv)) (by omega)
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def renderBranch : Fun.Branch → Doc
  | .branch _ pat body => group (renderPattern pat <+> atom "->" ++ nest 2 (line ++ renderExpr body))
termination_by b => sizeOf b

end

def renderFun (p : Fun.Program) : String :=
  renderDefs (p.definitions.map fun (_, name, body) => hangEq (atom "def" <+> renderName name) (renderExpr body))

/-! ## Core Full IR -/

mutual

def renderFullStmt : Full.Statement → Doc
  | .cut p c => renderFullProd p <+> atom "~" <+> renderFullCons c
  | .primitive _ name ps c =>
    atom "#" ++ fromString name ++ renderArgs (ps.map renderFullProd) <+> atom "~" <+> renderFullCons c
  | .invoke _ name c => atom "invoke" <+> renderName name <+> atom "~" <+> renderFullCons c
  | .externalCall _ name ps c =>
    atom "extern" <+> fromString name ++ renderArgs (ps.map renderFullProd) <+> atom "~" <+> renderFullCons c
  | .binOp _ op l r c =>
    parens (renderFullProd l <+> fromString op <+> renderFullProd r) <+> atom "~" <+> renderFullCons c
  | .ifz _ cond t e =>
    atom "if0" <+> renderFullProd cond <+> atom "then" <+> block (renderFullStmt t) <+> atom "else" <+> block (renderFullStmt e)
termination_by s => sizeOf s
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def renderFullProd : Full.Producer → Doc
  | .var _ name => renderName name
  | .literal _ lit => renderLit lit
  | .construct _ tag ps cs => renderTag tag ++ renderArgs ((ps.map renderFullProd) ++ (cs.map renderFullCons))
  | .lambda _ params stmt =>
    group (atom "\\" ++ hsep (params.map renderName) <+> atom "." <+> block (renderFullStmt stmt))
  | .object _ fields =>
    braceList ((sortAssocAscending fields).attach.map fun ⟨krs, hkrs⟩ =>
      fromString krs.1 ++ renderArgs [renderName krs.2.1] <+> atom "=" <+> block (renderFullStmt krs.2.2))
  | .«do» _ name stmt => atom "do" <+> renderName name <+> atom "." <+> block (renderFullStmt stmt)
  | .mu _ name stmt => atom "mu" <+> renderName name <+> atom "." <+> block (renderFullStmt stmt)
termination_by p => sizeOf p
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_snd_lt_of_mem (mem_sortAssocAscending hkrs)) (by omega)
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_snd_lt_of_mem hdvs) (by omega)
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def renderFullCons : Full.Consumer → Doc
  | .label _ name => renderName name
  | .apply _ ps cs => renderArgs ((ps.map renderFullProd) ++ (cs.map renderFullCons))
  | .project _ field c => atom "." ++ fromString field <+> atom "->" <+> renderFullCons c
  | .«then» _ name stmt => group (atom "then" <+> renderName name <+> atom "->" <+> block (renderFullStmt stmt))
  | .finish _ => atom "finish"
  | .select _ branches => atom "select" <+> blockLines (branches.map renderFullBranch)
termination_by c => sizeOf c
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def renderFullBranch : Full.Branch → Doc
  | .branch _ pat stmt => group (renderPattern pat <+> atom "->" ++ nest 2 (line ++ renderFullStmt stmt))
termination_by b => sizeOf b

end

def renderCoreFull (p : Full.Program) : String :=
  renderDefs (p.definitions.map fun d =>
    hangEq (atom "def" <+> renderName d.name ++ renderArgs [renderName d.ret]) (renderFullStmt d.body))

/-! ## Core Flat IR -/

mutual

def renderFlatStmt : Flat.Statement → Doc
  | .cut p c => renderFlatProd p <+> atom "~" <+> renderFlatCons c
  | .join _ name c s =>
    group (atom "join" <+> renderName name <+> atom "=" <+> renderFlatCons c) ++ line ++ atom "in" <+> renderFlatStmt s
  | .primitive _ name ps c =>
    atom "#" ++ fromString name ++ renderArgs (ps.map renderFlatProd) <+> atom "~" <+> renderFlatCons c
  | .invoke _ name c => atom "invoke" <+> renderName name <+> atom "~" <+> renderFlatCons c
  | .externalCall _ name ps c =>
    atom "extern" <+> fromString name ++ renderArgs (ps.map renderFlatProd) <+> atom "~" <+> renderFlatCons c
  | .binOp _ op l r c =>
    parens (renderFlatProd l <+> fromString op <+> renderFlatProd r) <+> atom "~" <+> renderFlatCons c
  | .ifz _ cond t e =>
    atom "if0" <+> renderFlatProd cond <+> atom "then" <+> block (renderFlatStmt t) <+> atom "else" <+> block (renderFlatStmt e)
termination_by s => sizeOf s
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def renderFlatProd : Flat.Producer → Doc
  | .var _ name => renderName name
  | .literal _ lit => renderLit lit
  | .construct _ tag ps cs => renderTag tag ++ renderArgs ((ps.map renderFlatProd) ++ (cs.map renderFlatCons))
  | .lambda _ params stmt =>
    group (atom "\\" ++ hsep (params.map renderName) <+> atom "." <+> block (renderFlatStmt stmt))
  | .object _ fields =>
    braceList ((sortAssocAscending fields).attach.map fun ⟨krs, hkrs⟩ =>
      fromString krs.1 ++ renderArgs [renderName krs.2.1] <+> atom "=" <+> block (renderFlatStmt krs.2.2))
  | .mu _ name stmt => atom "mu" <+> renderName name <+> atom "." <+> block (renderFlatStmt stmt)
termination_by p => sizeOf p
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_snd_lt_of_mem (mem_sortAssocAscending hkrs)) (by omega)
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_snd_lt_of_mem hdvs) (by omega)
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def renderFlatCons : Flat.Consumer → Doc
  | .label _ name => renderName name
  | .apply _ ps cs => renderArgs ((ps.map renderFlatProd) ++ (cs.map renderFlatCons))
  | .project _ field c => atom "." ++ fromString field <+> atom "->" <+> renderFlatCons c
  | .«then» _ name stmt => group (atom "then" <+> renderName name <+> atom "->" <+> block (renderFlatStmt stmt))
  | .finish _ => atom "finish"
  | .select _ branches => atom "select" <+> blockLines (branches.map renderFlatBranch)
termination_by c => sizeOf c
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def renderFlatBranch : Flat.Branch → Doc
  | .branch _ pat stmt => group (renderPattern pat <+> atom "->" ++ nest 2 (line ++ renderFlatStmt stmt))
termination_by b => sizeOf b

end

def renderFlat (p : Flat.Program) : String :=
  renderDefs (p.definitions.map fun d =>
    hangEq (atom "def" <+> renderName d.name ++ renderArgs [renderName d.ret]) (renderFlatStmt d.body))

/-! ## Core Join IR (consumer slots are plain names) -/

mutual

def renderJoinStmt : Join.Statement → Doc
  | .cut p ret => renderJoinProd p <+> atom "~" <+> renderName ret
  | .join _ name c s =>
    group (atom "join" <+> renderName name <+> atom "=" <+> renderJoinCons c) ++ line ++ atom "in" <+> renderJoinStmt s
  | .primitive _ name ps ret =>
    atom "#" ++ fromString name ++ renderArgs (ps.map renderJoinProd) <+> atom "~" <+> renderName ret
  | .invoke _ name ret => atom "invoke" <+> renderName name <+> atom "~" <+> renderName ret
  | .externalCall _ name ps ret =>
    atom "extern" <+> fromString name ++ renderArgs (ps.map renderJoinProd) <+> atom "~" <+> renderName ret
  | .binOp _ op l r ret =>
    parens (renderJoinProd l <+> fromString op <+> renderJoinProd r) <+> atom "~" <+> renderName ret
  | .ifz _ cond t e =>
    atom "if0" <+> renderJoinProd cond <+> atom "then" <+> block (renderJoinStmt t) <+> atom "else" <+> block (renderJoinStmt e)
termination_by s => sizeOf s
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def renderJoinProd : Join.Producer → Doc
  | .var _ name => renderName name
  | .literal _ lit => renderLit lit
  | .construct _ tag ps rets => renderTag tag ++ renderArgs ((ps.map renderJoinProd) ++ (rets.map renderName))
  | .lambda _ params stmt =>
    group (atom "\\" ++ hsep (params.map renderName) <+> atom "." <+> block (renderJoinStmt stmt))
  | .object _ fields =>
    braceList ((sortAssocAscending fields).attach.map fun ⟨krs, hkrs⟩ =>
      fromString krs.1 ++ renderArgs [renderName krs.2.1] <+> atom "=" <+> block (renderJoinStmt krs.2.2))
  | .mu _ name stmt => atom "mu" <+> renderName name <+> atom "." <+> block (renderJoinStmt stmt)
termination_by p => sizeOf p
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_snd_lt_of_mem (mem_sortAssocAscending hkrs)) (by omega)
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_snd_lt_of_mem hdvs) (by omega)
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def renderJoinCons : Join.Consumer → Doc
  | .label _ name => renderName name
  | .apply _ ps rets => renderArgs ((ps.map renderJoinProd) ++ (rets.map renderName))
  | .project _ field ret => atom "." ++ fromString field <+> atom "->" <+> renderName ret
  | .«then» _ name stmt => group (atom "then" <+> renderName name <+> atom "->" <+> block (renderJoinStmt stmt))
  | .finish _ => atom "finish"
  | .select _ branches => atom "select" <+> blockLines (branches.map renderJoinBranch)
termination_by c => sizeOf c
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def renderJoinBranch : Join.Branch → Doc
  | .branch _ pat stmt => group (renderPattern pat <+> atom "->" ++ nest 2 (line ++ renderJoinStmt stmt))
termination_by b => sizeOf b

end

def renderJoin (p : Join.Program) : String :=
  renderDefs (p.definitions.map fun d =>
    hangEq (atom "def" <+> renderName d.name ++ renderArgs [renderName d.ret]) (renderJoinStmt d.body))

/-! ## Zig backend ANF IR -/

def renderPath : Ir.Path → Doc
  | .root n => renderName n
  | .field p i => renderPath p ++ atom "." ++ atom (toString i)

def renderTest : Ir.Test → Doc
  | .kindIs path k => renderPath path ++ atom ".kind ==" <+> fromString k
  | .tagEq path tag => renderPath path ++ atom ".tag ==" <+> renderTag tag
  | .litEq path lit => renderPath path <+> atom "==" <+> renderLit lit

def renderGuard : Ir.Guard → Doc
  | .and tests => sep (punctuate (atom " &&") (tests.map renderTest))
  | .isZero v => renderName v <+> atom "== 0"

def renderIrExpr : Ir.Expr → Doc
  | .lit lit => renderLit lit
  | .mkStruct tag ops => renderTag tag ++ renderArgs (ops.map renderName)
  | .mkClosure fn ops => atom "closure" <+> renderName fn ++ renderArgs (ops.map renderName)
  | .mkRecord fields ops =>
    atom "record" ++ renderArgs (fields.map fun (f, fn) => fromString f <+> atom "=" <+> renderName fn)
      <+> atom "captures" ++ renderArgs (ops.map renderName)
  | .prim name ops => atom "#" ++ fromString name ++ renderArgs (ops.map renderName)
  | .readPath path => renderPath path
  | .readCapture self i => renderName self ++ atom ".cap" ++ list [atom (toString i)]
  | .force v field => renderName v ++ atom "!" ++ fromString field
  | .panicExpr msg => atom "panic" <+> dquotes (fromString msg)
  | .mkStructReuse tok tag ops => atom "reuse" <+> renderName tok <+> renderTag tag ++ renderArgs (ops.map renderName)

def renderStmt : Ir.Stmt → Doc
  | .let x e => atom "let" <+> renderName x <+> atom "=" <+> renderIrExpr e
  | .dup x => atom "dup" <+> renderName x
  | .drop x => atom "drop" <+> renderName x
  | .dropReuse tok x arity =>
    atom "dropReuse" <+> renderName tok <+> atom "=" <+> renderName x <+> atom "/" ++ atom (toString arity)

mutual

def renderBlock : Ir.Block → Doc
  | .mk stmts term => vsepHard ((stmts.map renderStmt) ++ [renderTerm term])

def renderTerm : Ir.Terminator → Doc
  | .applyCo k v => atom "return" <+> renderName k ++ renderArgs [renderName v]
  | .callClosure f args => atom "return" <+> renderName f ++ renderArgs (args.map renderName)
  | .staticCall fn args => atom "return" <+> renderName fn ++ renderArgs (args.map renderName)
  | .project v field k => atom "return" <+> renderName v ++ atom "." ++ fromString field ++ renderArgs [renderName k]
  | .«return» v => atom "return" <+> renderName v
  | .«if» guard t e =>
    group (atom "if" <+> renderGuard guard <+> atom "then" <+> block (renderBlock t) <+> atom "else" <+> block (renderBlock e))
  | .panic msg => atom "panic" <+> dquotes (fromString msg)

end

def renderKind : Ir.FuncKind → Doc
  | .topLevelFn => atom "toplevel"
  | .closureFn => atom "closure"
  | .fieldFn => atom "field"

def renderFunc (f : Ir.Func) : Doc :=
  let selfArg := match f.kind with
    | .topLevelFn => []
    | _ => [renderName f.selfVar]
  hangEq (renderKind f.kind <+> atom "fn" <+> renderName f.name ++ renderArgs (selfArg ++ (f.params.map renderName)))
    (renderBlock f.body)

def renderZigIr (p : Ir.Program) : String :=
  renderDefs ((p.funcs.map renderFunc) ++
    (match p.entry with
     | some e => [atom "entry" <+> atom "=" <+> renderName e]
     | none => []))

/-! ## Surface syntax (Parse/Rename) -/

def renderSynLit {k : Syntax.BoxKind} : Syntax.Literal k → Doc
  | .int32 n => atom (toString n.toInt)
  | .int64 n => atom (toString n.toInt) ++ atom "L"
  | .float f => atom (haskellShowFloat32 f) ++ atom "f"
  | .double d => atom (haskellShowFloat d)
  | .char c => squotes (fromString (String.singleton c))
  | .str s => dquotes (fromString s)

def renderAssoc : Syntax.Assoc → Doc
  | .leftA => atom "infixl"
  | .rightA => atom "infixr"
  | .neutralA => atom "infix"

def renderImportList : Syntax.ImportList → Doc
  | .all => .empty
  | .selected xs => renderArgs (xs.map fromString)
  | .«as» m' => atom "as" <+> fromString (pretty m')

def renderImport (m : ModuleName) (importList : Syntax.ImportList) : Doc :=
  atom "import" <+> fromString (pretty m) <+> renderImportList importList

mutual

def renderExprSyn {p : Syntax.Phase} [RenderId (Syntax.XId p)] : Syntax.Expr p → Doc
  | .var _ id => renderId id
  | .unboxed _ l => renderSynLit l
  | .boxed _ l => renderSynLit l
  | .apply _ e1 e2 => renderExprSyn e1 ++ renderArgs [renderExprSyn e2]
  | .opApp _ op e1 e2 => renderExprSyn e1 <+> renderId op <+> renderExprSyn e2
  | .project _ e k => renderExprSyn e ++ atom "." ++ fromString k
  | .fn _ cs =>
    atom "fn" <+> blockLines (punctuate (atom " |") (cs.toList.attach.map fun ⟨c, hc⟩ => renderClause c))
  | .tuple _ es => renderArgs (es.map renderExprSyn)
  | .record _ kvs =>
    braceList (kvs.attach.map fun ⟨kv, hkv⟩ => fromString kv.1 <+> atom "=" <+> renderExprSyn kv.2)
  | .list _ es => list (es.map renderExprSyn)
  | .ann _ e t => renderExprSyn e <+> atom ":" <+> renderType t
  | .seq _ ss => blockLines (ss.toList.attach.map fun ⟨s, hs⟩ => renderStmtSyn s)
  | .parens _ e => parens (renderExprSyn e)
  | .codata _ clauses =>
    atom "codata" <+> blockLines (clauses.attach.map fun ⟨ce, hce⟩ =>
      group (renderCoPat ce.1 <+> atom "->" ++ nest 2 (line ++ renderExprSyn ce.2)))
  | .label _ name body => atom "label" <+> renderId name <+> atom "." <+> renderExprSyn body
  | .goto _ value label => atom "goto" <+> renderExprSyn value <+> renderExprSyn label
termination_by e => sizeOf e
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_lt_of_mem hkv) (by omega)
    | exact Nat.lt_of_lt_of_le (sizeOf_fst_lt_of_mem hce) (by omega)
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_lt_of_mem hce) (by omega)
    | exact Nat.lt_of_lt_of_le (sizeOf_lt_of_mem_toList hc) (by omega)
    | exact Nat.lt_of_lt_of_le (sizeOf_lt_of_mem_toList hs) (by omega)
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def renderType {p : Syntax.Phase} [RenderId (Syntax.XId p)] : Syntax.Ty p → Doc
  | .app _ t ts => renderType t ++ renderArgs (ts.map renderType)
  | .var _ v => renderId v
  | .con _ c => renderId c
  | .arr _ t1 t2 => renderType t1 <+> atom "->" <+> renderType t2
  | .tuple _ ts => renderArgs (ts.map renderType)
  | .record _ kvs rowTail =>
    braceList ((kvs.attach.map fun ⟨kv, hkv⟩ => fromString kv.1 <+> atom ":" <+> renderType kv.2) ++
      (match rowTail with | some r => [atom "|" <+> renderType r] | none => []))
  | .block _ t => block (renderType t)
  | .bottom _ => atom "!"
  | .tilde _ t => atom "~" ++ renderType t
  | .variant _ cases rowTail =>
    list ((cases.attach.map fun ⟨kts, hkts⟩ =>
        fromString kts.1 ++ renderArgs (kts.2.attach.map fun ⟨t, ht⟩ => renderType t)) ++
      (match rowTail with | some r => [atom "|" <+> renderType r] | none => []))
termination_by t => sizeOf t
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_lt_of_mem hkv) (by omega)
    | exact Nat.lt_of_lt_of_le (sizeOf_lt_of_mem_snd_of_mem ht hkts) (by omega)
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def renderStmtSyn {p : Syntax.Phase} [RenderId (Syntax.XId p)] : Syntax.Stmt p → Doc
  | .letS _ var body => hangEq (atom "let" <+> renderId var) (renderExprSyn body)
  | .letPS _ pat body => hangEq (atom "let" <+> renderPat pat) (renderExprSyn body)
  | .withS _ none body => atom "with" <+> renderExprSyn body
  | .withS _ (some var) body => hangEq (atom "with" <+> renderId var) (renderExprSyn body)
  | .noBind _ body => renderExprSyn body
termination_by s => sizeOf s

def renderClause {p : Syntax.Phase} [RenderId (Syntax.XId p)] : Syntax.Clause p → Doc
  | .mk _ pats body =>
    group (hsep (pats.toList.attach.map fun ⟨pt, hpt⟩ => renderPat pt) <+> atom "->" ++
      nest 2 (line ++ renderExprSyn body))
termination_by c => sizeOf c
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_lt_of_mem_toList hpt) (by omega)

def renderPat {p : Syntax.Phase} [RenderId (Syntax.XId p)] : Syntax.Pat p → Doc
  | .var _ id => renderId id
  | .con _ id ps => renderId id ++ renderArgs (ps.map renderPat)
  | .tuple _ ps => renderArgs (ps.map renderPat)
  | .record _ kps =>
    braceList (kps.attach.map fun ⟨kp, hkp⟩ => fromString kp.1 <+> atom "=" <+> renderPat kp.2)
  | .list _ ps => list (ps.map renderPat)
  | .unboxed _ l => renderSynLit l
  | .boxed _ l => renderSynLit l
termination_by pt => sizeOf pt
decreasing_by
  all_goals simp_wf
  all_goals first
    | omega
    | exact Nat.lt_of_lt_of_le (sizeOf_snd_lt_of_mem hkp) (by omega)
    | (rename_i h
       exact Nat.lt_of_lt_of_le (List.sizeOf_lt_of_mem h) (by omega))

def renderCoPat {p : Syntax.Phase} [RenderId (Syntax.XId p)] : Syntax.CoPat p → Doc
  | .hole _ => atom "#"
  | .apply _ cp pt => renderCoPat cp ++ renderArgs [renderPat pt]
  | .project _ cp field => renderCoPat cp ++ atom "." ++ fromString field
termination_by cp => sizeOf cp

end

def renderScDef {p : Syntax.Phase} [RenderId (Syntax.XId p)] (f : Syntax.XId p) (e : Syntax.Expr p) : Doc :=
  hangEq (atom "def" <+> renderId f) (renderExprSyn e)

def renderScSig {p : Syntax.Phase} [RenderId (Syntax.XId p)] (f : Syntax.XId p) (t : Syntax.Ty p) : Doc :=
  atom "sig" <+> renderId f <+> atom ":" <+> renderType t

def renderDataDef {p : Syntax.Phase} [RenderId (Syntax.XId p)]
    (t : Syntax.XId p) (ps : List (Range × Syntax.XId p))
    (cons : List (Range × Syntax.XId p × List (Syntax.Ty p))) : Doc :=
  hangEq
    (atom "data" <+> renderId t <+> hsep (ps.map fun (_, x) => renderId x))
    (sep (punctuate (atom " |") (cons.map fun (_, con, ts) => renderId con ++ renderArgs (ts.map renderType))))

def renderTypeSynonym {p : Syntax.Phase} [RenderId (Syntax.XId p)]
    (t : Syntax.XId p) (ps : List (Syntax.XId p)) (ty : Syntax.Ty p) : Doc :=
  hangEq (atom "type" <+> renderId t <+> hsep (ps.map renderId)) (renderType ty)

def renderForeign {p : Syntax.Phase} [RenderId (Syntax.XId p)] (n : Syntax.XId p) (t : Syntax.Ty p) : Doc :=
  atom "foreign" <+> renderId n <+> atom ":" <+> renderType t

def renderDecl {p : Syntax.Phase} [RenderId (Syntax.XId p)] : Syntax.Decl p → Doc
  | .scDef _ f e => renderScDef f e
  | .scSig _ f t => renderScSig f t
  | .dataDef _ t ps cons => renderDataDef t ps cons
  | .typeSynonym _ t ps ty => renderTypeSynonym t ps ty
  | .«infix» _ assoc prec op => atom "infix" <+> renderAssoc assoc <+> atom (toString prec) <+> renderId op
  | .foreign _ n t => renderForeign n t
  | .«import» _ m importList => renderImport m importList

def renderParsedModule (m : Syntax.Module .parse) : String :=
  renderDefs (m.moduleDefinition.decls.map renderDecl)

def renderBindGroup (bg : Syntax.BindGroup .rename) : String :=
  renderDefs <|
    (bg.imports.map (fun (_, m, il) => renderImport m il)) ++
    (bg.typeSynonyms.map (fun (_, t, ps, ty) => renderTypeSynonym t ps ty)) ++
    (bg.dataDefs.map (fun (_, t, ps, cons) => renderDataDef t ps cons)) ++
    (bg.foreigns.map (fun (_, n, t) => renderForeign n t)) ++
    (bg.scSigs.map (fun (_, f, t) => renderScSig f t)) ++
    (bg.scDefs.flatMap (fun grp => grp.map (fun (_, f, e) => renderScDef f e)))

end Malgo.Debug.PrettyIR
