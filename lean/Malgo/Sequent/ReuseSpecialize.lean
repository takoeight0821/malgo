import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.Monad
import Malgo.Sequent.Fun

/-! Port of `src/Malgo/Sequent/ReuseSpecialize.hs`: an effectful
`Fun.Program → Fun.Program` rewrite run right after
`SaturateCtor.saturateProgram`. It recognizes self-recursive
"match / recurse / rebuild the same constructor" functions and inserts a
`reuseHint` primitive immediately before the reconstructed `Construct`,
after forcing every argument into its own binding (so the discarded
scrutinee's last use lands in the reconstruction's continuation — see the
Haskell module haddock for the full rationale and the M12/M14 limitation).

Effect mapping: Haskell `State Uniq` + `Reader ModuleName` becomes `MalgoM`
with the `ModuleName` threaded explicitly. No errors. -/

namespace Malgo.Sequent.ReuseSpecialize

open Malgo
open Malgo.Sequent.Fun

/-- Supports `partial def`s returning `MalgoM α` (inhabited via the monad's
error branch). -/
instance : Inhabited CompileError := ⟨{ passName := "", message := "" }⟩

/-! ## Pure analysis helpers -/

partial def occursInvoke (g : Name) : Expr → Bool
  | .var .. => false
  | .literal .. => false
  | .construct _ _ args => args.any (occursInvoke g)
  | .«let» _ _ v b => occursInvoke g v || occursInvoke g b
  | .lambda _ _ b => occursInvoke g b
  | .object _ fields => fields.any (fun (_, v) => occursInvoke g v)
  | .apply _ fn args => occursInvoke g fn || args.any (occursInvoke g)
  | .project _ e _ => occursInvoke g e
  | .primitive _ _ args => args.any (occursInvoke g)
  | .select _ scrutinee branches =>
    occursInvoke g scrutinee || branches.any (fun (.branch _ _ b) => occursInvoke g b)
  | .invoke _ n => n == g
  | .fix _ _ b => occursInvoke g b

def nonThunkOccurs (g : Name) : Expr → Bool
  | .lambda .. => false
  | a => occursInvoke g a

/-- Unroll an application spine outside-in (mirrors
`SaturateCtor.unrollSpine`). -/
def unrollSpine : Expr → List Expr → Expr × List Expr
  | .apply _ fn args, acc => unrollSpine fn (args ++ acc)
  | expr, acc => (expr, acc)

def rebuildSpine (r : Range) : Expr → List Expr → Expr
  | f, args => args.foldl (fun f a => Expr.apply r f [a]) f

def peelLambdas : Expr → List Name × Expr
  | .lambda _ [p] b => let (ps, b') := peelLambdas b; (p :: ps, b')
  | e => ([], e)

def scrutineeMatches : List Name → Expr → Bool
  | [p], .var _ v => v == p
  | ps, .construct _ .tuple args =>
    args.length == ps.length
      && (args.zip ps).all (fun (a, p) => match a with | .var _ v => v == p | _ => false)
  | _, _ => false

/-- The shape produced by `ToFun.fromClauses`: `n` nested single-argument
lambdas ending in `Select scrutinee branches`, where the scrutinee is the
lambda parameters. -/
def recognizeShape (body : Expr) : Option (List Name × Range × Expr × List Branch) := do
  let (params, inner) := peelLambdas body
  guard (!params.isEmpty)
  match inner with
  | .select scrutRange scrutinee branches =>
    if scrutineeMatches params scrutinee then some (params, scrutRange, scrutinee, branches)
    else none
  | _ => none

def rebuildLambdas (range : Range) (params : List Name) (body : Expr) : Expr :=
  params.foldr (fun p acc => .lambda range [p] acc) body

def lastPositionDestruct : Nat → Pattern → Option (Tag × List Pattern)
  | 1, .destruct _ tag pats => some (tag, pats)
  | n, .destruct _ .tuple pats =>
    if pats.length == n then
      match pats.reverse with
      | (.destruct _ tag fieldPats) :: _ => some (tag, fieldPats)
      | _ => none
    else none
  | _, _ => none

/-! ## Instrumentation (effectful, `partial`) -/

mutual

/-- Walk tail positions (through `Let`-chains and thunk-argument `Apply`
spines) and insert `reuseHint scrutinee` immediately before a tail
`Construct` matching the matched pattern's tag/arity with exactly one
recursive argument. Any other shape is left untouched (`none`). -/
partial def instrGo (mn : ModuleName) (g : Name) (tag : Tag) (arity : Nat) (scrutinee : Name) :
    Expr → MalgoM (Option Expr)
  | .«let» r n v b =>
    if occursInvoke g v then pure none
    else do
      let b? ← instrGo mn g tag arity scrutinee b
      pure (b?.map (fun b' => Expr.«let» r n v b'))
  | .apply r fn args => do
    let expr := Expr.apply r fn args
    let (headExpr, spineArgs) := unrollSpine expr []
    if occursInvoke g headExpr || spineArgs.any (nonThunkOccurs g) then pure none
    else do
      let rewrittenArgs ← spineArgs.mapM (instrRewriteArg mn g tag arity scrutinee)
      if rewrittenArgs.any Option.isSome then
        let newArgs := (spineArgs.zip rewrittenArgs).map (fun (a, r?) => r?.getD a)
        pure (some (rebuildSpine expr.range headExpr newArgs))
      else pure none
  | .construct r tag' args =>
    if tag' == tag && args.length == arity && (args.filter (occursInvoke g)).length == 1 then do
      let namedArgs ← args.mapM (fun val => do
        let n ← newTemporalId mn "reuseArg"
        pure (n, val))
      let hint ← newTemporalId mn "reuseHint"
      let reconstruct := Expr.construct r tag' (namedArgs.map (fun (n, _) => Expr.var r n))
      let withHint := Expr.«let» r hint
        (Expr.primitive r "reuseHint" [Expr.var r scrutinee]) reconstruct
      pure (some (namedArgs.foldr (fun (n, val) acc => Expr.«let» r n val acc) withHint))
    else pure none
  | _ => pure none

partial def instrRewriteArg (mn : ModuleName) (g : Name) (tag : Tag) (arity : Nat) (scrutinee : Name) :
    Expr → MalgoM (Option Expr)
  | .lambda lr [p] b => do
    let b? ← instrGo mn g tag arity scrutinee b
    pure (b?.map (fun b' => Expr.lambda lr [p] b'))
  | _ => pure none

end

def instrumentReconstructions (mn : ModuleName) (g : Name) (tag : Tag) (arity : Nat)
    (scrutinee : Name) (body : Expr) : MalgoM Expr := do
  let r? ← instrGo mn g tag arity scrutinee body
  pure (r?.getD body)

/-- Instrument a branch that destructures the last parameter via a flat,
all-`PVar` `Destruct`; otherwise leave it unchanged. -/
def instrumentBranchIfEligible (mn : ModuleName) (g : Name) (arity : Nat) (scrutinee : Name) :
    Branch → MalgoM Branch
  | .branch r pat body =>
    match lastPositionDestruct arity pat with
    | some (tag, fieldPats) =>
      if fieldPats.all (fun p => match p with | .pvar .. => true | _ => false) then do
        let body' ← instrumentReconstructions mn g tag fieldPats.length scrutinee body
        pure (.branch r pat body')
      else pure (.branch r pat body)
    | none => pure (.branch r pat body)

def specializeDef (mn : ModuleName) :
    Range × Name × Expr → MalgoM (Range × Name × Expr)
  | d@(range, g, body) =>
    match recognizeShape body with
    | none => pure d
    | some (params, scrutRange, scrutinee, branches) =>
      match params.reverse with
      | [] => pure d
      | lastParam :: _ => do
        let branches' ← branches.mapM (instrumentBranchIfEligible mn g params.length lastParam)
        pure (range, g, rebuildLambdas range params (.select scrutRange scrutinee branches'))

def specializeProgram (mn : ModuleName) (program : Program) : MalgoM Program := do
  let definitions ← program.definitions.mapM (specializeDef mn)
  pure { program with definitions }

end Malgo.Sequent.ReuseSpecialize
