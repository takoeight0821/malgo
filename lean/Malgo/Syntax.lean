import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.SExpr
import Malgo.Syntax.Extension
import Malgo.Data.Graph

/-! Port of `src/Malgo/Syntax.hs`: the phase-indexed surface AST.

Naming deviations forced by Lean keywords: `Type x` → `Ty p`,
`Stmt.Let/LetP/With/NoBind` → `letS/letPS/withS/noBind`. Constructor
names are otherwise the Haskell names in lowerCamel. -/

namespace Malgo.Syntax

inductive BoxKind where
  | boxed
  | unboxed

/-- Unboxed and boxed literal. The `BoxKind` phantom mirrors Haskell's
`Literal Unboxed`/`Literal Boxed` distinction. -/
inductive Literal (k : BoxKind) where
  | int32 (i : Int32)
  | int64 (i : Int64)
  | float (f : Float32)
  | double (d : Float)
  | char (c : Char)
  | str (s : String)
  deriving BEq, Repr

def Literal.toUnboxed : Literal .boxed → Literal .unboxed
  | .int32 i => .int32 i
  | .int64 i => .int64 i
  | .float f => .float f
  | .double d => .double d
  | .char c => .char c
  | .str s => .str s

instance : ToSExpr (Literal k) where
  toSExpr
    | .int32 i => .list [.atom (.symbol "int32"), .atom (.int i.toInt none)]
    | .int64 i => .list [.atom (.symbol "int64"), .atom (.int i.toInt none)]
    | .float f => .list [.atom (.symbol "float"), .atom (.float f)]
    | .double d => .list [.atom (.symbol "double"), .atom (.double d)]
    | .char c => .list [.atom (.symbol "char"), .atom (.char c)]
    | .str s => .list [.atom (.symbol "string"), .atom (.str s)]

mutual

inductive CoPat (p : Phase) where
  | hole (ext : XHoleP p)
  | apply (ext : XApplyP p) (copat : CoPat p) (pat : Pat p)
  | project (ext : XProjectP p) (copat : CoPat p) (field : String)

inductive Ty (p : Phase) where
  | app (ext : XTyApp p) (ty : Ty p) (args : List (Ty p))
  | var (ext : XTyVar p) (name : XId p)
  | con (ext : XTyCon p) (name : XId p)
  | arr (ext : XTyArr p) (dom cod : Ty p)
  | tuple (ext : XTyTuple p) (tys : List (Ty p))
  | record (ext : XTyRecord p) (fields : List (String × Ty p)) (rowTail : Option (Ty p))
  | block (ext : XTyBlock p) (ty : Ty p)
  | bottom (ext : XTyBottom p)
  | tilde (ext : XTyTilde p) (ty : Ty p)
  | variant (ext : XTyVariant p) (cases : List (String × List (Ty p))) (rowTail : Option (Ty p))

inductive Expr (p : Phase) where
  | var (ext : XVar p) (name : XId p)
  | unboxed (ext : XUnboxed p) (lit : Literal .unboxed)
  | boxed (ext : XBoxed p) (lit : Literal .boxed)
  | apply (ext : XApply p) (fn arg : Expr p)
  | opApp (ext : XOpApp p) (op : XId p) (lhs rhs : Expr p)
  | project (ext : XProject p) (expr : Expr p) (field : String)
  | fn (ext : XFn p) (clauses : NEList (Clause p))
  | tuple (ext : XTuple p) (exprs : List (Expr p))
  | record (ext : XRecord p) (fields : List (String × Expr p))
  | list (ext : XList p) (exprs : List (Expr p))
  | ann (ext : XAnn p) (expr : Expr p) (ty : Ty p)
  | seq (ext : XSeq p) (stmts : NEList (Stmt p))
  | parens (ext : XParens p) (expr : Expr p)
  | codata (ext : XCodata p) (clauses : List (CoPat p × Expr p))
  | label (ext : XLabel p) (name : XId p) (body : Expr p)
  | goto (ext : XGoto p) (value label : Expr p)

inductive Stmt (p : Phase) where
  | letS (ext : XLet p) (name : XId p) (expr : Expr p)
  /-- `let pat = e` with a non-variable pattern; Rename desugars it. -/
  | letPS (ext : XLetP p) (pat : Pat p) (expr : Expr p)
  | withS (ext : XWith p) (name : Option (XId p)) (expr : Expr p)
  | noBind (ext : XNoBind p) (expr : Expr p)

inductive Clause (p : Phase) where
  | mk (ext : XClause p) (pats : NEList (Pat p)) (body : Expr p)

inductive Pat (p : Phase) where
  | var (ext : XVarP p) (name : XId p)
  | con (ext : XConP p) (name : XId p) (pats : List (Pat p))
  | tuple (ext : XTupleP p) (pats : List (Pat p))
  | record (ext : XRecordP p) (fields : List (String × Pat p))
  | list (ext : XListP p) (pats : List (Pat p))
  | unboxed (ext : XUnboxedP p) (lit : Literal .unboxed)
  | boxed (ext : XBoxedP p) (lit : Literal .boxed)

end

abbrev CoClause (p : Phase) := CoPat p × Expr p

inductive Decl (p : Phase) where
  | scDef (ext : XScDef p) (name : XId p) (expr : Expr p)
  | scSig (ext : XScSig p) (name : XId p) (ty : Ty p)
  | dataDef (ext : XDataDef p) (name : XId p) (params : List (Range × XId p))
      (cons : List (Range × XId p × List (Ty p)))
  | typeSynonym (ext : XTypeSynonym p) (name : XId p) (params : List (XId p)) (ty : Ty p)
  | «infix» (ext : XInfix p) (assoc : Assoc) (prec : Int) (op : XId p)
  | foreign (ext : XForeign p) (name : XId p) (ty : Ty p)
  | «import» (ext : XImport p) (moduleName : ModuleName) (importList : ImportList)

-- Haskell type synonyms for BindGroup components (tuples, accessed by position).
abbrev ScDef (p : Phase) := XScDef p × XId p × Expr p
abbrev ScSig (p : Phase) := XScSig p × XId p × Ty p
abbrev DataDef (p : Phase) :=
  XDataDef p × XId p × List (Range × XId p) × List (Range × XId p × List (Ty p))
abbrev TypeSynonym (p : Phase) := XTypeSynonym p × XId p × List (XId p) × Ty p
abbrev Foreign (p : Phase) := XForeign p × XId p × Ty p
abbrev Import (p : Phase) := XImport p × ModuleName × ImportList

structure ParsedDefinitions (p : Phase) where
  decls : List (Decl p)

/-- Top-level bindings grouped by mutual recursion (`scDefs` is a list of
strongly connected groups, dependencies first). -/
structure BindGroup (p : Phase) where
  scDefs : List (List (ScDef p))
  scSigs : List (ScSig p)
  dataDefs : List (DataDef p)
  typeSynonyms : List (TypeSynonym p)
  foreigns : List (Foreign p)
  imports : List (Import p)

/-- Haskell's open family `XModule`: parse-phase modules hold raw decl
lists, renamed modules hold the bind group. -/
abbrev XModule : Phase → Type
  | .parse => ParsedDefinitions .parse
  | .rename => BindGroup .rename

structure Module (p : Phase) where
  moduleName : ModuleName
  moduleDefinition : XModule p

/-! ## Ranges -/

def Expr.range : {p : Phase} → Expr p → Range
  | _, .var ext _ => ext
  | _, .unboxed ext _ => ext
  | .parse, .boxed ext _ => ext
  | .rename, .boxed ext _ => nomatch ext
  | _, .apply ext _ _ => ext
  | .parse, .opApp ext _ _ _ => ext
  | .rename, .opApp ext _ _ _ => ext.1
  | _, .project ext _ _ => ext
  | _, .fn ext _ => ext
  | _, .tuple ext _ => ext
  | _, .record ext _ => ext
  | .parse, .list ext _ => ext
  | .rename, .list ext _ => nomatch ext
  | _, .ann ext _ _ => ext
  | _, .seq ext _ => ext
  | _, .parens ext _ => ext
  | _, .codata ext _ => ext
  | _, .label ext _ _ => ext
  | _, .goto ext _ _ => ext

instance : HasRange (Expr p) := ⟨Expr.range⟩

def CoPat.range : CoPat p → Range
  | .hole ext => ext
  | .apply ext _ _ => ext
  | .project ext _ _ => ext

instance : HasRange (CoPat p) := ⟨CoPat.range⟩

def Pat.range : {p : Phase} → Pat p → Range
  | _, .var ext _ => ext
  | _, .con ext _ _ => ext
  | _, .tuple ext _ => ext
  | _, .record ext _ => ext
  | .parse, .list ext _ => ext
  | .rename, .list ext _ => nomatch ext
  | _, .unboxed ext _ => ext
  | .parse, .boxed ext _ => ext
  | .rename, .boxed ext _ => nomatch ext

instance : HasRange (Pat p) := ⟨Pat.range⟩

def Ty.range : {p : Phase} → Ty p → Range
  | _, .app ext _ _ => ext
  | _, .var ext _ => ext
  | .parse, .con ext _ => nomatch ext
  | .rename, .con ext _ => ext
  | _, .arr ext _ _ => ext
  | _, .tuple ext _ => ext
  | _, .record ext _ _ => ext
  | .parse, .block ext _ => ext
  | .rename, .block ext _ => nomatch ext
  | _, .bottom ext => ext
  | _, .tilde ext _ => ext
  | _, .variant ext _ _ => ext

instance : HasRange (Ty p) := ⟨Ty.range⟩

def Stmt.range : {p : Phase} → Stmt p → Range
  | _, .letS ext _ _ => ext
  | .parse, .letPS ext _ _ => ext
  | .rename, .letPS ext _ _ => nomatch ext
  | .parse, .withS ext _ _ => ext
  | .rename, .withS ext _ _ => nomatch ext
  | _, .noBind ext _ => ext

instance : HasRange (Stmt p) := ⟨Stmt.range⟩

def Clause.range : Clause p → Range
  | .mk ext _ _ => ext

instance : HasRange (Clause p) := ⟨Clause.range⟩

/-! ## S-expression dumps (the golden contract) -/

private def sym (s : String) : SExpr := .atom (.symbol s)

mutual

partial def CoPat.dump [ToSExpr (XId p)] : CoPat p → SExpr
  | .hole _ => sym "#"
  | .apply _ cp pat => .list [sym "apply", cp.dump, pat.dump]
  | .project _ cp field => .list [sym "project", cp.dump, .atom (.str field)]

partial def Ty.dump [ToSExpr (XId p)] : Ty p → SExpr
  | .app _ t ts => .list [sym "app", t.dump, .list (ts.map Ty.dump)]
  | .var _ v => toSExpr v
  | .con _ c => toSExpr c
  | .arr _ t1 t2 => .list [sym "->", t1.dump, t2.dump]
  | .tuple _ ts => .list (sym "tuple" :: ts.map Ty.dump)
  | .record _ kvs rowTail =>
    .list (sym "record" :: kvs.map (fun (k, v) => .list [toSExpr k, v.dump]) ++
      (rowTail.map fun r => [SExpr.list [sym "row", r.dump]]).getD [])
  | .block _ t => .list [sym "block", t.dump]
  | .bottom _ => sym "_|_"
  | .tilde _ t => .list [sym "~", t.dump]
  | .variant _ cases rowTail =>
    .list (sym "variant" :: cases.map (fun (k, ts) => .list (toSExpr k :: ts.map Ty.dump)) ++
      (rowTail.map fun r => [SExpr.list [sym "row", r.dump]]).getD [])

partial def Expr.dump [ToSExpr (XId p)] : Expr p → SExpr
  | .var _ id => toSExpr id
  | .unboxed _ l => toSExpr l
  | .boxed _ l => toSExpr l
  | .apply _ e1 e2 => .list [sym "apply", e1.dump, e2.dump]
  | .opApp _ op e1 e2 => .list [sym "opapp", toSExpr op, e1.dump, e2.dump]
  | .project _ e k => .list [sym "project", e.dump, .atom (.str k)]
  | .fn _ cs => .list [sym "fn", .list (cs.toList.map Clause.dump)]
  | .tuple _ es => .list (sym "tuple" :: es.map Expr.dump)
  | .record _ kvs => .list (sym "record" :: kvs.map fun (k, v) => .list [toSExpr k, v.dump])
  | .list _ es => .list (sym "list" :: es.map Expr.dump)
  | .ann _ e t => .list [sym "ann", e.dump, t.dump]
  | .seq _ ss => .list (sym "seq" :: ss.toList.map Stmt.dump)
  | .parens _ e => .list [sym "parens", e.dump]
  | .codata _ clauses =>
    .list (sym "codata" :: clauses.map fun (cp, e) => .list [cp.dump, e.dump])
  | .label _ name body => .list [sym "label", toSExpr name, body.dump]
  | .goto _ value label => .list [sym "goto", value.dump, label.dump]

partial def Stmt.dump [ToSExpr (XId p)] : Stmt p → SExpr
  | .letS _ id e => .list [sym "let", toSExpr id, e.dump]
  | .letPS _ pat e => .list [sym "let", pat.dump, e.dump]
  | .withS _ none e => .list [sym "with", e.dump]
  | .withS _ (some id) e => .list [sym "with", toSExpr id, e.dump]
  | .noBind _ e => .list [sym "do", e.dump]

partial def Clause.dump [ToSExpr (XId p)] : Clause p → SExpr
  | .mk _ pats body => .list [sym "clause", .list (pats.toList.map Pat.dump), body.dump]

partial def Pat.dump [ToSExpr (XId p)] : Pat p → SExpr
  | .var _ id => toSExpr id
  | .con _ id ps => .list [sym "con", toSExpr id, .list (ps.map Pat.dump)]
  | .tuple _ ps => .list (sym "tuple" :: ps.map Pat.dump)
  | .record _ kps => .list (sym "record" :: kps.map fun (k, pat) => .list [toSExpr k, pat.dump])
  | .list _ ps => .list (sym "list" :: ps.map Pat.dump)
  | .unboxed _ l => .list [sym "unboxed", toSExpr l]
  | .boxed _ l => .list [sym "boxed", toSExpr l]

end

instance [ToSExpr (XId p)] : ToSExpr (CoPat p) := ⟨CoPat.dump⟩
instance [ToSExpr (XId p)] : ToSExpr (Ty p) := ⟨Ty.dump⟩
instance [ToSExpr (XId p)] : ToSExpr (Expr p) := ⟨Expr.dump⟩
instance [ToSExpr (XId p)] : ToSExpr (Stmt p) := ⟨Stmt.dump⟩
instance [ToSExpr (XId p)] : ToSExpr (Clause p) := ⟨Clause.dump⟩
instance [ToSExpr (XId p)] : ToSExpr (Pat p) := ⟨Pat.dump⟩

def Decl.dump [ToSExpr (XId p)] : Decl p → SExpr
  | .scDef _ f e => .list [sym "def", toSExpr f, toSExpr e]
  | .scSig _ f t => .list [sym "sig", toSExpr f, toSExpr t]
  | .dataDef _ t ps cons =>
    .list [sym "data", toSExpr t,
      .list (ps.map fun (_, p) => toSExpr p),
      .list (cons.map fun (_, c, ts) => .list [toSExpr c, .list (ts.map toSExpr)])]
  | .typeSynonym _ t ps ty =>
    .list [sym "type", toSExpr t, .list (ps.map toSExpr), toSExpr ty]
  | .«infix» _ assoc prec op =>
    .list [sym "infix", toSExpr assoc, .atom (.int prec none), toSExpr op]
  | .foreign _ n t => .list [sym "foreign", toSExpr n, toSExpr t]
  | .«import» _ m list => .list [sym "import", toSExpr m, toSExpr list]

instance [ToSExpr (XId p)] : ToSExpr (Decl p) := ⟨Decl.dump⟩

instance [ToSExpr (XId p)] : ToSExpr (ParsedDefinitions p) :=
  ⟨fun pd => .list (pd.decls.map toSExpr)⟩

instance [ToSExpr (XId p)] : ToSExpr (BindGroup p) where
  toSExpr bg :=
    .list
      [ .list (bg.scDefs.map fun group =>
          SExpr.list (group.map fun (_, f, e) => .list [sym "def", toSExpr f, toSExpr e])),
        .list (bg.scSigs.map fun (_, f, t) => .list [sym "sig", toSExpr f, toSExpr t]),
        .list (bg.dataDefs.map fun (_, name, ps, cons) =>
          .list [sym "data", toSExpr name,
            .list (ps.map fun (_, p) => toSExpr p),
            .list (cons.map fun (_, c, ts) => .list [toSExpr c, .list (ts.map toSExpr)])]),
        .list (bg.typeSynonyms.map fun (_, name, ps, ty) =>
          .list [sym "type", toSExpr name, .list (ps.map toSExpr), toSExpr ty]),
        .list (bg.foreigns.map fun (_, n, t) => .list [sym "foreign", toSExpr n, toSExpr t]),
        .list (bg.imports.map fun (_, m, list) => .list [sym "import", toSExpr m, toSExpr list]) ]

instance [ToSExpr (XId p)] [ToSExpr (XModule p)] : ToSExpr (Module p) where
  toSExpr m := .list [sym "module", toSExpr m.moduleName, toSExpr m.moduleDefinition]

/-! ## Free variables and bound variables -/

/-- Variables bound by a pattern. -/
partial def Pat.boundVars [Ord (XId p)] : Pat p → Std.TreeSet (XId p)
  | .var _ x => ({} : Std.TreeSet (XId p)).insert x
  | .con _ _ ps => ps.foldl (fun acc pat => acc.merge pat.boundVars) {}
  | .tuple _ ps => ps.foldl (fun acc pat => acc.merge pat.boundVars) {}
  | .record _ kps => kps.foldl (fun acc (_, pat) => acc.merge pat.boundVars) {}
  | .list _ ps => ps.foldl (fun acc pat => acc.merge pat.boundVars) {}
  | .unboxed _ _ => {}
  | .boxed _ _ => {}

private partial def CoPat.boundVars [Ord (XId p)] : CoPat p → Std.TreeSet (XId p)
  | .hole _ => {}
  | .apply _ cp pat => cp.boundVars.merge pat.boundVars
  | .project _ cp _ => cp.boundVars

mutual

partial def Expr.freevars [Ord (XId p)] : Expr p → Std.TreeSet (XId p)
  | .var _ v => ({} : Std.TreeSet (XId p)).insert v
  | .unboxed _ _ => {}
  | .boxed _ _ => {}
  | .apply _ e1 e2 => e1.freevars.merge e2.freevars
  | .opApp _ op e1 e2 => (e1.freevars.merge e2.freevars).insert op
  | .project _ e _ => e.freevars
  | .fn _ cs =>
    cs.toList.foldl (init := {}) fun acc c =>
      match c with
      | .mk _ pats e =>
        acc.merge <|
          pats.toList.foldl (fun fv pat => fv.eraseMany pat.boundVars.toList) e.freevars
  | .tuple _ es => es.foldl (fun acc e => acc.merge e.freevars) {}
  | .record _ kvs => kvs.foldl (fun acc (_, e) => acc.merge e.freevars) {}
  | .list _ es => es.foldl (fun acc e => acc.merge e.freevars) {}
  | .ann _ e _ => e.freevars
  | .seq _ ss => freevarsStmts ss.head ss.tail
  | .parens _ e => e.freevars
  | .codata _ clauses =>
    clauses.foldl (init := {}) fun acc (copat, e) =>
      acc.merge (e.freevars.eraseMany copat.boundVars.toList)
  | .label _ name body => body.freevars.erase name
  | .goto _ value label => value.freevars.merge label.freevars

private partial def freevarsStmts [Ord (XId p)] (s : Stmt p) (ss : List (Stmt p)) :
    Std.TreeSet (XId p) :=
  let rest := match ss with
    | [] => {}
    | s' :: ss' => freevarsStmts s' ss'
  match s with
  | .letS _ x e => e.freevars.merge (rest.erase x)
  | .letPS _ pat e => e.freevars.merge (rest.eraseMany pat.boundVars.toList)
  | .withS _ none e => e.freevars.merge rest
  | .withS _ (some x) e => e.freevars.merge (rest.erase x)
  | .noBind _ e => e.freevars.merge rest

end

/-! ## Bind groups -/

/-- Port of `makeBindGroup`: classify decls and split `scDefs` into
strongly connected groups (SCC order mirrors `Data.Graph`; uniq
assignment downstream depends on it). -/
def makeBindGroup [Ord (XId p)] [BEq (XId p)] (ds : List (Decl p)) : BindGroup p :=
  let scDefList := ds.filterMap fun
    | .scDef x f e => some (x, f, e)
    | _ => none
  let sccs := Data.sccGroups <| scDefList.map fun (_, f, e) =>
    (f, (e.freevars.erase f).toList)
  { scDefs := sccs.map fun group =>
      group.filterMap fun n => scDefList.find? fun (_, f, _) => n == f
    scSigs := ds.filterMap fun
      | .scSig x f t => some (x, f, t)
      | _ => none
    dataDefs := ds.filterMap fun
      | .dataDef x t ps cons => some (x, t, ps, cons)
      | _ => none
    typeSynonyms := ds.filterMap fun
      | .typeSynonym x t ps ty => some (x, t, ps, ty)
      | _ => none
    foreigns := ds.filterMap fun
      | .foreign x n t => some (x, n, t)
      | _ => none
    imports := ds.filterMap fun
      | .«import» x m ns => some (x, m, ns)
      | _ => none }

end Malgo.Syntax
