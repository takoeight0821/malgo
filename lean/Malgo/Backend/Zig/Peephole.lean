import Malgo.Backend.Zig.Ir
import Malgo.Prelude
import Malgo.Sequent.Fun

/-! Port of `src/Malgo/Backend/Zig/Peephole.hs`: scrutinee-tuple
elimination on the backend IR.

Every multi-parameter clause match is compiled as a `Select` on a fresh
tuple of the parameters, reaching this IR as a `MkStruct` whose only
consumers are `ReadPath` reads and guard `Test`s. This pass removes such
tuples (re-rooting reads/tests at the tuple's operands and deleting
statically-true tests against the tuple itself), then substitutes away the
pure aliases (`Let x = ReadPath (PRoot v)`) left behind. Runs BEFORE
Perceus, so the input carries no `Dup`/`Drop`. -/

namespace Malgo.Backend.Zig.Peephole

open Malgo.Sequent.Fun (Name Tag)
open Malgo.Backend.Zig.Ir

instance : Inhabited Ir.Path := ⟨.root default⟩
instance : Inhabited Expr := ⟨.panicExpr ""⟩
instance : Inhabited Stmt := ⟨.drop default⟩
instance : Inhabited Terminator := ⟨.panic ""⟩
instance : Inhabited Block := ⟨.mk [] (.panic "")⟩

/-! ## Tuple elimination -/

/-- `t.fields[i]...` becomes a path rooted at the `i`-th operand. A bare
`PRoot t` (the tuple as a whole) cannot be re-rooted. -/
def reroot (ops : List Name) : Ir.Path → Option Ir.Path
  | .root _ => none
  | .field (.root _) i =>
      match ops.drop i with
      | op :: _ => some (.root op)
      | [] => none
  | .field p i => (reroot ops p).map (.field · i)

/-- `none` aborts the elimination; `some none` deletes a statically-true
test; `some (some test)` keeps a (re-rooted) test. -/
def elimUsesTest (t : Name) (tag : Tag) (ops : List Name) (test : Test) : Option (Option Test) :=
  match test with
  | .tagEq (.root root) tag' =>
      if root == t then (if tag' == tag then some none else none)
      else some (some test)
  | .kindIs (.root root) kindName =>
      if root == t then (if kindName == "strukt" then some none else none)
      else some (some test)
  | _ =>
      if test.path.root' != t then some (some test)
      else match test with
        | .tagEq p tag' => (reroot ops p).map (fun p' => some (.tagEq p' tag'))
        | .kindIs p kindName => (reroot ops p).map (fun p' => some (.kindIs p' kindName))
        | .litEq p l => (reroot ops p).map (fun p' => some (.litEq p' l))

/- Rewrite every use of tuple `t` (constructed as `MkStruct tag ops`) in
the block, or `none` if any use is not a re-rootable read/test. -/
mutual

partial def elimUses (t : Name) (tag : Tag) (ops : List Name) : Block → Option Block
  | .mk stmts terminator => do
      let (stmts', terminator') ← euStmts t tag ops stmts terminator
      pure (.mk stmts' terminator')

partial def euStmts (t : Name) (tag : Tag) (ops : List Name) :
    List Stmt → Terminator → Option (List Stmt × Terminator)
  | [], terminator => (euTerm t tag ops terminator).map (fun tm => ([], tm))
  | stmt :: rest, terminator =>
      let keep := (euStmts t tag ops rest terminator).map (fun (ss, tm) => (stmt :: ss, tm))
      match stmt with
      | .let x (.readPath p) =>
          if p.root' == t then
            match reroot ops p with
            | none => none
            | some p' =>
                (euStmts t tag ops rest terminator).map
                  (fun (ss, tm) => (.let x (.readPath p') :: ss, tm))
          else keep
      | .let _ e => if (freeVarsExpr e).contains t then none else keep
      | .dup x => if x == t then none else keep
      | .drop x => if x == t then none else keep
      | _ => keep

partial def euTerm (t : Name) (tag : Tag) (ops : List Name) : Terminator → Option Terminator
  | .«if» guard thenB elseB => do
      let g ← euGuard t tag ops guard
      let tb ← elimUses t tag ops thenB
      let eb ← elimUses t tag ops elseB
      pure (.«if» g tb eb)
  | term =>
      if (freeVarsTerminator term).contains t then none else some term

partial def euGuard (t : Name) (tag : Tag) (ops : List Name) : Guard → Option Guard
  | .and tests =>
      (tests.mapM (elimUsesTest t tag ops)).map (fun opts => .and (opts.filterMap id))
  | .isZero v => if v == t then none else some (.isZero v)

end

mutual

partial def elimTuples : Block → Block
  | .mk stmts terminator => elimTuplesGo stmts terminator

partial def elimTuplesGo : List Stmt → Terminator → Block
  | [], terminator =>
      match terminator with
      | .«if» guard t e => .mk [] (.«if» guard (elimTuples t) (elimTuples e))
      | term => .mk [] term
  | stmt :: rest, terminator =>
      match stmt with
      | .let tName (.mkStruct tag ops) =>
          match elimUses tName tag ops (.mk rest terminator) with
          | some (.mk rest' terminator') => elimTuples (.mk rest' terminator')
          | none =>
              match elimTuplesGo rest terminator with
              | .mk rest' terminator' => .mk (stmt :: rest') terminator'
      | _ =>
          match elimTuplesGo rest terminator with
          | .mk rest' terminator' => .mk (stmt :: rest') terminator'

end

/-! ## Alias elimination -/

def snPath (frm to : Name) : Ir.Path → Ir.Path
  | .root v => .root (if v == frm then to else v)
  | .field p i => .field (snPath frm to p) i

def snTest (frm to : Name) : Test → Test
  | .kindIs p k => .kindIs (snPath frm to p) k
  | .tagEq p tag => .tagEq (snPath frm to p) tag
  | .litEq p l => .litEq (snPath frm to p) l

def snGuard (frm to : Name) : Guard → Guard
  | .and tests => .and (tests.map (snTest frm to))
  | .isZero v => .isZero (if v == frm then to else v)

def snExpr (frm to : Name) (e : Expr) : Expr :=
  let rn := fun x => if x == frm then to else x
  match e with
  | .lit l => .lit l
  | .mkStruct tag vs => .mkStruct tag (vs.map rn)
  | .mkClosure fn vs => .mkClosure fn (vs.map rn)
  | .mkRecord fields vs => .mkRecord fields (vs.map rn)
  | .prim name vs => .prim name (vs.map rn)
  | .readPath p => .readPath (snPath frm to p)
  | .readCapture self i => .readCapture (rn self) i
  | .force v field => .force (rn v) field
  | .panicExpr what => .panicExpr what
  | .mkStructReuse .. =>
      panic! "Malgo.Backend.Zig.Peephole: input already contains MkStructReuse (Reuse runs after Peephole)"

def snStmt (frm to : Name) : Stmt → Stmt
  | .let x e => .let x (snExpr frm to e)
  | .dup x => .dup (if x == frm then to else x)
  | .drop x => .drop (if x == frm then to else x)
  | .dropReuse .. =>
      panic! "Malgo.Backend.Zig.Peephole: input already contains DropReuse (Reuse runs after Peephole)"

mutual

partial def substNameBlock (frm to : Name) : Block → Block
  | .mk stmts term => .mk (stmts.map (snStmt frm to)) (snTerm frm to term)

partial def snTerm (frm to : Name) : Terminator → Terminator
  | .«if» guard t e => .«if» (snGuard frm to guard) (substNameBlock frm to t) (substNameBlock frm to e)
  | term =>
      let rn := fun x => if x == frm then to else x
      match term with
      | .applyCo k v => .applyCo (rn k) (rn v)
      | .callClosure f args => .callClosure (rn f) (args.map rn)
      | .staticCall fn args => .staticCall fn (args.map rn)
      | .project v field k => .project (rn v) field (rn k)
      | .«return» v => .«return» (rn v)
      | .«if» .. => term  -- unreachable (handled above)
      | .panic msg => .panic msg

end

mutual

partial def elimAliases : Block → Block
  | .mk stmts terminator => elimAliasesGo stmts terminator

partial def elimAliasesGo : List Stmt → Terminator → Block
  | [], terminator =>
      match terminator with
      | .«if» guard t e => .mk [] (.«if» guard (elimAliases t) (elimAliases e))
      | term => .mk [] term
  | .let x (.readPath (.root v)) :: rest, terminator =>
      elimAliases (substNameBlock x v (.mk rest terminator))
  | stmt :: rest, terminator =>
      match elimAliasesGo rest terminator with
      | .mk rest' terminator' => .mk (stmt :: rest') terminator'

end

/-- Iterated to a fixpoint: eliminating an outer tuple re-roots reads at
its operands, which can expose an inner tuple or a fresh alias for the
next round. -/
partial def peepholeFunc (fn : Func) : Func :=
  let body' := elimAliases (elimTuples fn.body)
  if body' == fn.body then fn
  else peepholeFunc { fn with body := body' }

def peepholeProgram (program : Program) : Program :=
  { program with funcs := program.funcs.map peepholeFunc }

private def nm (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }
private def r0 : Range := { start := SourcePos.initial "", stop := SourcePos.initial "" }

-- A two-parameter clause match's scrutinee tuple `t = MkStruct [a, b]`,
-- with a field read `x = t.fields[0]`, collapses to `return a`: the
-- tuple's `MkStruct` is deleted, the read is re-rooted at `a`, and the
-- resulting alias `x = ReadPath (root a)` is substituted away.
private def peepFn : Func :=
  { range := r0, name := nm "f", kind := .topLevelFn, selfVar := nm "self",
    params := [nm "a", nm "b"],
    body := .mk
      [ .let (nm "t") (.mkStruct .tuple [nm "a", nm "b"]),
        .let (nm "x") (.readPath (.field (.root (nm "t")) 0)) ]
      (.«return» (nm "x")) }

#guard (peepholeFunc peepFn).body == Block.mk [] (.«return» (nm "a"))

/-! ## Exact-shape unit checks (port of `test/Malgo/Backend/Zig/PeepholeSpec.hs`)

Corpus-wide safety is covered by the zig-golden parity harness; these pin the
exact rewrite, including the four cases where the pass must *not* fire.
`#guard` rather than golden cases: they are pure structural equalities, so
`lake build` is the right place to catch a regression. -/

private def tnm (s : String) (uniq : Nat) : Name :=
  { name := s, moduleName := .moduleName "PeepholeTest", sort := .temporal uniq }

private def vA : Name := tnm "a" 10
private def vB : Name := tnm "b" 11
private def vK : Name := tnm "k" 12
private def vH : Name := tnm "h" 13
private def vT : Name := tnm "t" 14
private def vU : Name := tnm "u" 15

private def peepBody (params : List Name) (body : Block) : Block :=
  (peepholeFunc { range := r0, name := tnm "fn" 0, kind := .topLevelFn,
                  selfVar := tnm "self" 1, params, body }).body

-- The shape `fromClauses` produces: a fresh tuple of the parameters, with a
-- guarded Select arm reading fields back out of it. Reads are re-rooted, the
-- now-trivially-true root test is dropped, and the alias is substituted.
#guard peepBody [vA, vK]
    (.mk [.let vT (.mkStruct .tuple [vA, vK]), .let vH (.readPath (.field (.root vT) 0))]
      (.«if» (.and [.tagEq (.root vT) .tuple, .tagEq (.field (.root vT) 0) (.tag "Cons")])
        (.mk [] (.applyCo vK vH)) (.mk [] (.panic "no match"))))
  == .mk [] (.«if» (.and [.tagEq (.root vA) (.tag "Cons")])
      (.mk [] (.applyCo vK vA)) (.mk [] (.panic "no match")))

-- Re-rooting is not limited to one field deep.
#guard peepBody [vA, vK]
    (.mk [.let vT (.mkStruct .tuple [vA, vK]),
          .let vH (.readPath (.field (.field (.root vT) 0) 1))]
      (.applyCo vK vH))
  == .mk [.let vH (.readPath (.field (.root vA) 1))] (.applyCo vK vH)

-- Must NOT fire: the tuple escapes into a call, so it has to be built.
#guard peepBody [vA, vK] (.mk [.let vT (.mkStruct .tuple [vA])] (.applyCo vK vT))
  == .mk [.let vT (.mkStruct .tuple [vA])] (.applyCo vK vT)

-- Must NOT fire: a primitive observes the tuple as a value.
#guard peepBody [vA, vK]
    (.mk [.let vT (.mkStruct .tuple [vA]), .let vH (.prim "malgo_print" [vT])] (.applyCo vK vH))
  == .mk [.let vT (.mkStruct .tuple [vA]), .let vH (.prim "malgo_print" [vT])] (.applyCo vK vH)

-- Must NOT fire: the root test asks for a tag the construction does not have,
-- so it is not trivially true and the tuple is genuinely inspected.
#guard peepBody [vA, vK]
    (.mk [.let vT (.mkStruct .tuple [vA])]
      (.«if» (.and [.tagEq (.root vT) (.tag "Cons")])
        (.mk [] (.applyCo vK vT)) (.mk [] (.panic "no match"))))
  == .mk [.let vT (.mkStruct .tuple [vA])]
      (.«if» (.and [.tagEq (.root vT) (.tag "Cons")])
        (.mk [] (.applyCo vK vT)) (.mk [] (.panic "no match")))

-- Both arms of a TIf read from the same tuple.
#guard peepBody [vA, vB, vK]
    (.mk [.let vT (.mkStruct .tuple [vA, vB])]
      (.«if» (.isZero vA)
        (.mk [.let vH (.readPath (.field (.root vT) 0))] (.applyCo vK vH))
        (.mk [.let vU (.readPath (.field (.root vT) 1))] (.applyCo vK vU))))
  == .mk [] (.«if» (.isZero vA) (.mk [] (.applyCo vK vA)) (.mk [] (.applyCo vK vB)))

-- Nested tuples collapse via the fixpoint, not just one layer.
#guard peepBody [vA, vB, vK]
    (.mk [.let (tnm "inner" 20) (.mkStruct .tuple [vA, vB]),
          .let vT (.mkStruct .tuple [tnm "inner" 20]),
          .let vH (.readPath (.field (.field (.root vT) 0) 1))]
      (.applyCo vK vH))
  == .mk [] (.applyCo vK vB)

-- A pure alias is substituted even with no tuple in sight.
#guard peepBody [vA, vK] (.mk [.let vH (.readPath (.root vA))] (.applyCo vK vH))
  == .mk [] (.applyCo vK vA)

end Malgo.Backend.Zig.Peephole
