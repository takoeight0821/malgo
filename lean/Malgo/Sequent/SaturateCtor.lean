import Std.Data.TreeMap
import Malgo.Prelude
import Malgo.Id
import Malgo.Sequent.Fun

/-! Port of `src/Malgo/Sequent/SaturateCtor.hs`: a pure `Fun.Program →
Fun.Program` rewrite run at the very start of `ToCore.toCore` (before CPS
conversion). It recognizes curried-constructor definitions structurally and
rewrites a fully-(or over-)saturated call spine into a direct
`Fun.Construct`, leaving genuine partial applications untouched. Shared by
every backend and every direct caller of `toCore`. -/

namespace Malgo.Sequent.SaturateCtor

open Malgo.Sequent.Fun

/-- Structural test for a curried-constructor definition
(`\p1 -> .. -> \pn -> Construct tag [p1..pn]`). Also matches plain
tuple-returning functions of the same shape (sound to inline either way). -/
partial def peel (params : List Name) : Expr → Option (Tag × Nat)
  | .lambda _ [p] b => peel (params ++ [p]) b
  | .construct _ tag args =>
    if args.length == params.length
        && (args.zip params).all (fun (a, p) => match a with | .var _ n => n == p | _ => false)
    then some (tag, params.length) else none
  | _ => none

def recognizeDef : Range × Name × Expr → Option (Name × Tag × Nat)
  | (_, name, body) => (peel [] body).map (fun (tag, n) => (name, tag, n))

/-- Unroll an application spine outside-in, preserving left-to-right
argument order: `Apply (Apply f [a1]) [a2]` becomes `(f, [a1, a2])`. -/
def unrollSpine : Expr → List Expr → Expr × List Expr
  | .apply _ fn args, acc => unrollSpine fn (args ++ acc)
  | expr, acc => (expr, acc)

/-- A saturated/over-saturated call of a recognized constructor becomes a
`Construct` (with any excess arguments re-applied). Under-saturated calls
stay as-is (`none`). -/
def trySaturate (ctorTable : Std.TreeMap Name (Tag × Nat)) (expr : Expr) : Option Expr := do
  let (base, args) := unrollSpine expr []
  let name ← match base with | .invoke _ n => some n | _ => none
  let (tag, arity) ← ctorTable.get? name
  guard (args.length ≥ arity)
  let ctorArgs := args.take arity
  let extra := args.drop arity
  let built := Expr.construct expr.range tag ctorArgs
  pure (extra.foldl (fun f a => Expr.apply expr.range f [a]) built)

mutual

partial def goExpr (ctorTable : Std.TreeMap Name (Tag × Nat)) (expr : Expr) : Expr :=
  let expr' := match expr with
    | .var .. => expr
    | .literal .. => expr
    | .construct r tag args => .construct r tag (args.map (goExpr ctorTable))
    | .«let» r n v b => .«let» r n (goExpr ctorTable v) (goExpr ctorTable b)
    | .lambda r ps b => .lambda r ps (goExpr ctorTable b)
    | .object r fields => .object r (fields.map (fun (k, v) => (k, goExpr ctorTable v)))
    | .apply r fn args => .apply r (goExpr ctorTable fn) (args.map (goExpr ctorTable))
    | .project r e field => .project r (goExpr ctorTable e) field
    | .primitive r op args => .primitive r op (args.map (goExpr ctorTable))
    | .select r s branches => .select r (goExpr ctorTable s) (branches.map (goBranch ctorTable))
    | .invoke .. => expr
    | .fix r n b => .fix r n (goExpr ctorTable b)
  match trySaturate ctorTable expr' with
  | some rewritten => rewritten
  | none => expr'

partial def goBranch (ctorTable : Std.TreeMap Name (Tag × Nat)) : Branch → Branch
  | .branch r p b => .branch r p (goExpr ctorTable b)

end

def saturateProgram (program : Program) : Program :=
  let ctorTable : Std.TreeMap Name (Tag × Nat) :=
    (program.definitions.filterMap recognizeDef).foldl
      (fun m (name, tp) => m.insert name tp) {}
  { program with
    definitions := program.definitions.map (fun (r, n, b) => (r, n, goExpr ctorTable b)) }

/-! Sanity check: a saturated `Cons a b` call (via `Invoke`) collapses into
a direct `Construct`. -/
section Test
private def r0 : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
private def extId (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }
private def testProg : Program :=
  { definitions :=
      [ (r0, extId "Cons",
          .lambda r0 [extId "x"] (.lambda r0 [extId "y"]
            (.construct r0 (.tag "Cons") [.var r0 (extId "x"), .var r0 (extId "y")]))),
        (r0, extId "main",
          .apply r0 (.apply r0 (.invoke r0 (extId "Cons")) [.var r0 (extId "a")])
            [.var r0 (extId "b")]) ],
    dependencies := [] }

#guard
  match (saturateProgram testProg).definitions with
  | [_, (_, _, body)] => Malgo.sShow body == "(Cons (a b))"
  | _ => false
end Test

end Malgo.Sequent.SaturateCtor
