import Std.Data.TreeSet
import Std.Data.TreeMap
import Malgo.Prelude
import Malgo.Id
import Malgo.Module
import Malgo.Monad
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Fun
import Malgo.Backend.Zig.Ir
import Malgo.Sequent.Core.Normalize
import Malgo.Sequent.Core.Escape
import Malgo.Backend.Zig.Stage

/-! Port of `src/Malgo/Backend/Zig/ClosureConv.hs`: closure conversion and
lambda lifting from (normalized) Join IR to the first-order `Ir.Program`.

Every nested `Lambda`/escaping `Join`/`Object` field becomes its own
top-level `Ir.Func`; the enclosing-scope names each body references become
explicit indexed `ReadCapture` reads off the function's `self`, with the
`MkClosure`/`MkRecord` call site supplying the captured values in the same
index order.

Fidelity notes vs. the Haskell:
  * `Data.Set Name` → `Std.TreeSet Name`; `Set.toList` (ascending by `Ord
    Name`) → `.toList`, so capture ordering matches Haskell's ascending
    order. `Data.Map` → `Std.TreeMap`; `Map.union`/`Map.unions` are
    left-biased (Haskell's `<>`), reproduced by `mapUnion`/`mapUnions`
    below via `insertIfNew`.
  * Fresh names come from `Malgo.newTemporalId moduleName`; the effect
    order (uniq numbering) is translated call-for-call left-to-right so a
    future golden-parity pass sees the same numbering the Haskell produces.
  * `error`-style "cannot happen" cases (Mu in producer position, a
    non-variable record sub-pattern) become `throw` of a `CompileError`. -/

namespace Malgo.Backend.Zig.ClosureConv

open Malgo.Sequent.Core.Join
open Malgo.Sequent.Fun (Name Literal Tag Pattern)
open Malgo.Sequent.Core.Normalize (normalizeStatement substStatement)
open Malgo.Sequent.Core.Escape


/-! ## Pattern compilation -/

/-- The renamer can assign the same wildcard `Id` to more than one surface
parameter; `Ir` binders must be unique per path, so every repeat is
replaced with a fresh (never referenced) name. -/
partial def dedupParams (moduleName : ModuleName) (seen : Std.TreeSet Name) :
    List Name → MalgoM (List Name)
  | [] => pure []
  | p :: ps =>
    if seen.contains p then do
      let p' ← Malgo.newTemporalId moduleName "unused_param"
      let rest ← dedupParams moduleName seen ps
      pure (p' :: rest)
    else do
      let rest ← dedupParams moduleName (seen.insert p) ps
      pure (p :: rest)

mutual

/-- Compile a pattern match against a scrutinee path into borrowing guard
tests and the bindings to splice before the branch body. Mirrors
`Eval.match`/`matchDL`. -/
partial def compilePatternGo (moduleName : ModuleName) (seen : Std.TreeSet Name) (path : Ir.Path) :
    Pattern → MalgoM (List Ir.Test × List Ir.Stmt × Std.TreeSet Name)
  | .pvar _ name =>
    if seen.contains name then pure ([], [], seen)
    else pure ([], [Ir.Stmt.«let» name (Ir.Expr.readPath path)], seen.insert name)
  | .pliteral _ lit => pure ([Ir.Test.litEq path lit], [], seen)
  | .destruct _ tag pats => do
    let (subTests, binds, seen') ← compilePatternFields moduleName path seen 0 pats
    pure (Ir.Test.tagEq path tag :: subTests, binds, seen')
  | .expand _ fieldPats => do
    let (recordBinds, recordVar) ← match path with
      | .root v => pure ([], v)
      | _ => do
        let r ← Malgo.newTemporalId moduleName "record"
        pure ([Ir.Stmt.«let» r (Ir.Expr.readPath path)], r)
    -- Fields forced in ascending name order, matching Eval's matchExpand.
    let (binds, seen') ← compilePatternExpand moduleName recordVar seen (sortAssocAscending fieldPats)
    pure ([Ir.Test.kindIs path "record"], recordBinds ++ binds, seen')

partial def compilePatternFields (moduleName : ModuleName) (path : Ir.Path)
    (seen : Std.TreeSet Name) (i : Nat) :
    List Pattern → MalgoM (List Ir.Test × List Ir.Stmt × Std.TreeSet Name)
  | [] => pure ([], [], seen)
  | p :: rest => do
    let (g1, b1, seen1) ← compilePatternGo moduleName seen (Ir.Path.field path i) p
    let (g2, b2, seen2) ← compilePatternFields moduleName path seen1 (i + 1) rest
    pure (g1 ++ g2, b1 ++ b2, seen2)

partial def compilePatternExpand (moduleName : ModuleName) (recordVar : Name)
    (seen : Std.TreeSet Name) :
    List (String × Pattern) → MalgoM (List Ir.Stmt × Std.TreeSet Name)
  | [] => pure ([], seen)
  | (fieldName, p) :: rest => do
    let (bind, seen1) ← match p with
      | .pvar _ name =>
        if seen.contains name then do
          -- A repeated wildcard Id: the force still runs (the field may
          -- have effects), bound to a fresh dead name.
          let t ← Malgo.newTemporalId moduleName "expand"
          pure ([Ir.Stmt.«let» t (Ir.Expr.force recordVar fieldName)], seen)
        else pure ([Ir.Stmt.«let» name (Ir.Expr.force recordVar fieldName)], seen.insert name)
      | _ =>
        throw { passName := "ClosureConv",
                message := "only variable patterns are supported inside record patterns" }
    let (restBinds, seen2) ← compilePatternExpand moduleName recordVar seen1 rest
    pure (bind ++ restBinds, seen2)

end

def compilePatternIr (moduleName : ModuleName) (scrut : Name) (pat : Pattern) :
    MalgoM (List Ir.Test × List Ir.Stmt) := do
  let (tests, binds, _) ← compilePatternGo moduleName {} (Ir.Path.root scrut) pat
  pure (tests, binds)

/-! ## Closure conversion proper -/

mutual

partial def convertStatement (moduleName : ModuleName) (env : LocalEnv) (ownership : OwnershipMap) :
    Statement → MalgoM (Ir.Block × List Ir.Func)
  | .cut producer k => do
    let (stmts, v, fns1) ← convertProducer moduleName env ownership producer
    let (block, fns2) ← match env.get? k with
      | some consumer => convertApply moduleName env ownership consumer v
      | none => pure (Ir.Block.mk [] (.applyCo k v), [])
    pure (Ir.Block.mk (stmts ++ block.stmts) block.terminator, fns1 ++ fns2)
  | .join joinRange name consumer stmt =>
    match ownership.getD name Ownership.Local with
    | .Local => convertStatement moduleName (env.insert name consumer) ownership stmt
    | .Escaping => do
      let (allocStmts, fns1) ← liftJoinConsumer moduleName joinRange name consumer
      let (block, fns2) ← convertStatement moduleName env ownership stmt
      pure (Ir.Block.mk (allocStmts ++ block.stmts) block.terminator, fns1 ++ fns2)
  | .primitive _ name producers k => do
    let (stmts, vs, fns1) ← convertProducers moduleName env ownership producers
    let result ← Malgo.newTemporalId moduleName "prim_result"
    let bind := Ir.Stmt.«let» result (Ir.Expr.prim name vs)
    let (block, fns2) ← match env.get? k with
      | some consumer => convertApply moduleName env ownership consumer result
      | none => pure (Ir.Block.mk [] (.applyCo k result), [])
    pure (Ir.Block.mk (stmts ++ (bind :: block.stmts)) block.terminator, fns1 ++ fns2)
  | .invoke _ name k => pure (Ir.Block.mk [] (.staticCall name [k]), [])
  | .externalCall exRange name producers k =>
    convertStatement moduleName env ownership (.primitive exRange name producers k)
  | .binOp opRange op lhs rhs k =>
    convertStatement moduleName env ownership (.primitive opRange op [lhs, rhs] k)
  | .ifz _ cond t e => do
    let (stmts, v, fns1) ← convertProducer moduleName env ownership cond
    let (tBlock, fns2) ← convertStatement moduleName env ownership t
    let (eBlock, fns3) ← convertStatement moduleName env ownership e
    pure (Ir.Block.mk stmts (.«if» (.isZero v) tBlock eBlock), fns1 ++ fns2 ++ fns3)

/-- Apply a `Consumer` to an already-available variable, inline in the
current function scope. -/
partial def convertApply (moduleName : ModuleName) (env : LocalEnv) (ownership : OwnershipMap)
    (consumer : Consumer) (v : Name) : MalgoM (Ir.Block × List Ir.Func) :=
  match consumer with
  | .label _ name => pure (Ir.Block.mk [] (.applyCo name v), [])
  | .apply _ producers returns => do
    let (stmts, vs, fns) ← convertProducers moduleName env ownership producers
    pure (Ir.Block.mk stmts (.callClosure v (vs ++ returns)), fns)
  | .project _ field k => pure (Ir.Block.mk [] (.project v field k), [])
  -- The bound name is renamed to the already-available variable instead of
  -- emitting an alias binding — sound because every Id is globally unique.
  | .«then» _ name stmt => convertStatement moduleName env ownership (substStatement name v stmt)
  | .finish _ => pure (Ir.Block.mk [] (.«return» v), [])
  | .select _ branches => convertSelect moduleName env ownership v branches

partial def convertProducers (moduleName : ModuleName) (env : LocalEnv) (ownership : OwnershipMap) :
    List Producer → MalgoM (List Ir.Stmt × List Name × List Ir.Func)
  | [] => pure ([], [], [])
  | p :: ps => do
    let (stmts, v, fns1) ← convertProducer moduleName env ownership p
    let (restStmts, vs, fns2) ← convertProducers moduleName env ownership ps
    pure (stmts ++ restStmts, v :: vs, fns1 ++ fns2)

partial def convertProducer (moduleName : ModuleName) (env : LocalEnv) (ownership : OwnershipMap) :
    Producer → MalgoM (List Ir.Stmt × Name × List Ir.Func)
  | .var _ x => pure ([], x, [])
  | .literal _ lit => do
    let t ← Malgo.newTemporalId moduleName "lit"
    pure ([Ir.Stmt.«let» t (Ir.Expr.lit lit)], t, [])
  | .construct _ tag producers returns => do
    let (stmts, vs, fns) ← convertProducers moduleName env ownership producers
    let t ← Malgo.newTemporalId moduleName "struct"
    pure (stmts ++ [Ir.Stmt.«let» t (Ir.Expr.mkStruct tag (vs ++ returns))], t, fns)
  | .lambda lamRange params stmt => do
    let fnId ← Malgo.newTemporalId moduleName "lambda"
    let selfVar ← Malgo.newTemporalId moduleName "self"
    let captures := ((freeVarsStatement stmt).diff (Std.TreeSet.ofList params)).toList
    let captureLets := captures.mapIdx (fun i c => Ir.Stmt.«let» c (Ir.Expr.readCapture selfVar i))
    let params' ← dedupParams moduleName {} params
    let (block, fns) ← convertStatement moduleName {} (classifyJoins stmt) stmt
    let fn : Ir.Func :=
      { range := lamRange, name := fnId, kind := .closureFn, selfVar, params := params',
        body := Ir.Block.mk (captureLets ++ block.stmts) block.terminator }
    let t ← Malgo.newTemporalId moduleName "closure"
    pure ([Ir.Stmt.«let» t (Ir.Expr.mkClosure fnId captures)], t, fn :: fns)
  | .object objRange fields => do
    -- A record's fields share ONE captured environment (mirroring the
    -- evaluator's `Record env fields`): the allocation's capture list is
    -- the union of every field's free variables, but each field function
    -- reads only its own subset out of the shared list.
    let sharedCaptures :=
      (nameSetUnions (fields.map (fun (_, ret, stmt) => (freeVarsStatement stmt).erase ret))).toList
    let indexedCaptures := sharedCaptures.zipIdx
    let fieldResults ← fields.mapM (fun (fieldName, retName, stmt) => do
      let fnId ← Malgo.newTemporalId moduleName "field"
      let selfVar ← Malgo.newTemporalId moduleName "self"
      let usedByThisField := (freeVarsStatement stmt).erase retName
      let captureLets := indexedCaptures.filterMap (fun (c, i) =>
        if usedByThisField.contains c then
          some (Ir.Stmt.«let» c (Ir.Expr.readCapture selfVar i))
        else none)
      let (block, fns) ← convertStatement moduleName {} (classifyJoins stmt) stmt
      let fn : Ir.Func :=
        { range := objRange, name := fnId, kind := .fieldFn, selfVar, params := [retName],
          body := Ir.Block.mk (captureLets ++ block.stmts) block.terminator }
      pure ((fieldName, fnId), fn :: fns))
    let t ← Malgo.newTemporalId moduleName "record"
    pure ([Ir.Stmt.«let» t (Ir.Expr.mkRecord (fieldResults.map Prod.fst) sharedCaptures)],
      t, fieldResults.flatMap Prod.snd)
  | .mu _ _ _ =>
    throw { passName := "ClosureConv",
            message := "Mu in producer position should have been eliminated by Normalize" }

/-- Lift an `Escaping` join's consumer into its own `Ir.Func`, returning the
allocation statement that binds the join's name to the closure. -/
partial def liftJoinConsumer (moduleName : ModuleName)
    (joinRange : Range) (name : Name) (consumer : Consumer) :
    MalgoM (List Ir.Stmt × List Ir.Func) := do
  let fnId ← Malgo.newTemporalId moduleName "join"
  let selfVar ← Malgo.newTemporalId moduleName "self"
  let valueParam ← Malgo.newTemporalId moduleName "value"
  let captures := (freeVarsConsumer consumer).toList
  let captureLets := captures.mapIdx (fun i c => Ir.Stmt.«let» c (Ir.Expr.readCapture selfVar i))
  -- The join fixpoint must be recomputed in this fresh scope using only the
  -- joins collected from this consumer's body.
  let (block, fns) ← convertApply moduleName {} (classifyJoinsConsumer consumer) consumer valueParam
  let fn : Ir.Func :=
    { range := joinRange, name := fnId, kind := .closureFn, selfVar, params := [valueParam],
      body := Ir.Block.mk (captureLets ++ block.stmts) block.terminator }
  pure ([Ir.Stmt.«let» name (Ir.Expr.mkClosure fnId captures)], fn :: fns)

partial def convertSelect (moduleName : ModuleName) (env : LocalEnv) (ownership : OwnershipMap)
    (scrut : Name) : List Branch → MalgoM (Ir.Block × List Ir.Func)
  | [] => pure (Ir.Block.mk [] (.panic "no matching branch"), [])
  | .branch _ pat stmt :: rest => do
    let (tests, binds) ← compilePatternIr moduleName scrut pat
    let (body, fns1) ← convertStatement moduleName env ownership stmt
    let (elseBlock, fns2) ← convertSelect moduleName env ownership scrut rest
    pure (Ir.Block.mk []
      (.«if» (.and tests) (Ir.Block.mk (binds ++ body.stmts) body.terminator) elseBlock),
      fns1 ++ fns2)

end

/-! ## Program entry -/

def convertDefinition (moduleName : ModuleName) (d : Definition) : MalgoM (List Ir.Func) := do
  -- Normalize once per top-level definition: `normalizeStatement` recurses
  -- into every nested body, so this single call covers the whole tree.
  let stmt := normalizeStatement d.body
  let selfVar ← Malgo.newTemporalId moduleName "self"
  let (block, lifted) ← convertStatement moduleName {} (classifyJoins stmt) stmt
  pure ({ range := d.range, name := d.name, kind := .topLevelFn, selfVar, params := [d.ret],
          body := block : Ir.Func } :: lifted)

/-- Keep only the first definition for each `Id`, dropping later occurrences
reached via a different import path to the same module. -/
def nubByName : List Definition → List Definition :=
  go {}
where
  go (seen : Std.TreeSet Name) : List Definition → List Definition
    | [] => []
    | d :: rest =>
      if seen.contains d.name then go seen rest
      else d :: go (seen.insert d.name) rest

/-- Find the definition named `main`, matching `Eval.evalProgram`'s
bootstrap: the first definition whose `Id.name` is exactly `"main"`. -/
def findMain (defs : List Definition) : Option (Range × Name) :=
  (defs.filterMap (fun d => if d.name.name == "main" then some (d.range, d.name) else none)).head?

/-- Build the program's entry-point statement, mirroring
`Eval.evalProgram`'s bootstrap: run `Invoke main afterMain` where
`afterMain`, handed `main`'s value (a closure), applies it to unit and a
fresh `Finish` continuation. -/
def mainEntryStatement (moduleName : ModuleName) (entryRange : Range) (mainName : Name) :
    MalgoM Statement := do
  let finishName ← Malgo.newTemporalId moduleName "finish"
  let afterMain ← Malgo.newTemporalId moduleName "after_main"
  pure <|
    .join entryRange finishName (.finish entryRange) <|
      .join entryRange afterMain
          (.apply entryRange [.construct entryRange .tuple [] []] [finishName]) <|
        .invoke entryRange mainName afterMain

/-- Convert a linked Join IR program into the backend `Ir.Program`. -/
def convertProgram (moduleName : ModuleName) (program : Program) : MalgoM (Staged .closureConv) := do
  -- A module reachable via more than one import path appears once per path
  -- in `Program.definitions`; deduplicate by `Id` (stable across paths).
  let definitions := nubByName program.definitions
  let defFuncs ← definitions.mapM (convertDefinition moduleName)
  let defFuncsFlat := defFuncs.flatten
  -- A module with no top-level `main` compiles to a no-op executable,
  -- matching `Eval.evalProgram`.
  match findMain definitions with
  | none => pure ⟨{ funcs := defFuncsFlat, entry := none }⟩
  | some (entryRange, mainName) => do
    let entryStmt ← mainEntryStatement moduleName entryRange mainName
    let fnName ← Malgo.newTemporalId moduleName "zig_main"
    let selfVar ← Malgo.newTemporalId moduleName "self"
    let (block, lifted) ← convertStatement moduleName {} (classifyJoins entryStmt) entryStmt
    let fn : Ir.Func :=
      { range := entryRange, name := fnName, kind := .topLevelFn, selfVar, params := [],
        body := block }
    pure ⟨{ funcs := defFuncsFlat ++ (fn :: lifted), entry := some fnName }⟩

section Test
private def r0 : Range := ⟨SourcePos.initial "", SourcePos.initial ""⟩
private def nm (s : String) : Name := { name := s, moduleName := .moduleName "t", sort := .external }

-- A `Cut` references both the producer's variable and the continuation.
#guard (freeVarsStatement (.cut (.var r0 (nm "x")) (nm "k"))).toList.map (·.name) == ["k", "x"]
-- A `Join`-bound name is not free once bound.
#guard (freeVarsStatement (.join r0 (nm "j") (.finish r0) (.cut (.var r0 (nm "x")) (nm "j")))).toList.map (·.name)
  == ["x"]
-- One join is collected, keyed by its name.
#guard (collectJoins (.join r0 (nm "j") (.finish r0) (.cut (.var r0 (nm "x")) (nm "j")))).map (fun (n, _) => n.name)
  == ["j"]
-- A join whose continuation never lets its name escape stays `Local`.
#guard (classifyJoins (.join r0 (nm "j") (.finish r0) (.cut (.var r0 (nm "x")) (nm "j")))).get? (nm "j")
  == some Ownership.Local
-- A join whose name is passed to `Invoke` (a cross-unit call) escapes.
#guard (classifyJoins (.join r0 (nm "j") (.finish r0) (.invoke r0 (nm "f") (nm "j")))).get? (nm "j")
  == some Ownership.Escaping
end Test

end Malgo.Backend.Zig.ClosureConv
