import Std.Data.TreeSet
import Std.Data.TreeMap
import Malgo.Prelude
import Malgo.Id
import Malgo.Sequent.Core.Join
import Malgo.Sequent.Fun

/-! Escape analysis for Join IR: decide which `Statement.join` bindings need
to become real values and which can be compiled away.

A join-bound consumer is `Local` when every use of its name is a position
that transfers control to it *here and now* — `cut`'s consumer, and the
consumer of `primitive`/`externalCall`/`binOp`. It is `Escaping` when the
name is instead stored or handed to someone else: `invoke`'s consumer,
`apply`'s or `construct`'s consumer list, `project`'s return, or a free
variable of a nested `lambda`/`object`/`mu` body. `promoteEscapingCaptures`
then closes the set under "referenced by an escaping join's consumer".

Both backends want this, for different reasons. Zig *needs* it: with no GC
and no closures, an escaping join has to be lifted to its own function with
an explicit captures array. Go is already correct without it — it makes
every join a Go closure — and uses it purely to stop allocating one, and to
stop bouncing the trampoline, for the ~35% of joins that are `Local`.

Two preconditions, both currently guaranteed by how Join IR is built:

* `Normalize` must run first. It removes `Cut (Mu ..) ..` and the
  `Join m (Label j)` forwarding form, so `Consumer.label` never reaches
  here as a join's consumer.
* A join may not reference itself. `initialClassifyJoinsWithEscaping`
  decides a binder's ownership from the escape set of its scope *body*
  only, so a self-referencing `Local` join would make an inlining consumer
  loop forever. `tellJoin` (`Sequent/Core/Join.lean`) mints the name after
  building the consumer, so this cannot arise.

The analysis is per function scope and must be recomputed for each one —
that is what makes "free variable of a nested lambda ⇒ escaping" correct,
since a name crossing a function boundary has to be a runtime value. -/

namespace Malgo.Sequent.Core.Escape

open Malgo.Sequent.Core.Join
open Malgo.Sequent.Fun (Name Pattern)

/-- Whether a `Join`-bound consumer name can be compiled as an inline
substitution within its defining function, or must be reified as a
heap-allocated closure value. -/
inductive Ownership where
  | Local
  | Escaping
  deriving BEq

abbrev OwnershipMap := Std.TreeMap Name Ownership
abbrev LocalEnv := Std.TreeMap Name Consumer

private def emptySet : Std.TreeSet Name := {}
private def sing (x : Name) : Std.TreeSet Name := Std.TreeSet.ofList [x]
def nameSetUnions (xs : List (Std.TreeSet Name)) : Std.TreeSet Name :=
  xs.foldl (fun acc s => acc.union s) emptySet

/-- Left-biased `Map.union` (Haskell's `<>` on `Map`): keeps `a`'s value on
a shared key. -/
private def mapUnion (a b : OwnershipMap) : OwnershipMap :=
  b.foldl (fun acc k v => acc.insertIfNew k v) a
private def mapUnions (xs : List OwnershipMap) : OwnershipMap :=
  xs.foldl mapUnion {}

/-! ## Free variables -/

partial def patternVars : Pattern → Std.TreeSet Name
  | .pvar _ name => sing name
  | .pliteral _ _ => emptySet
  | .destruct _ _ pats => nameSetUnions (pats.map patternVars)
  | .expand _ fields => nameSetUnions (fields.map (fun (_, p) => patternVars p))

mutual

partial def freeVarsStatement : Statement → Std.TreeSet Name
  | .cut p k => (freeVarsProducer p).insert k
  | .join _ name consumer stmt =>
    (freeVarsConsumer consumer).union ((freeVarsStatement stmt).erase name)
  | .primitive _ _ ps k => (nameSetUnions (ps.map freeVarsProducer)).insert k
  | .invoke _ _ k => sing k
  | .externalCall _ _ ps k => (nameSetUnions (ps.map freeVarsProducer)).insert k
  | .binOp _ _ lhs rhs k => ((freeVarsProducer lhs).union (freeVarsProducer rhs)).insert k
  | .ifz _ cond t e =>
    (freeVarsProducer cond).union ((freeVarsStatement t).union (freeVarsStatement e))

partial def freeVarsProducer : Producer → Std.TreeSet Name
  | .var _ name => sing name
  | .literal _ _ => emptySet
  | .construct _ _ ps ks =>
    (nameSetUnions (ps.map freeVarsProducer)).union (Std.TreeSet.ofList ks)
  | .lambda _ names stmt => (freeVarsStatement stmt).diff (Std.TreeSet.ofList names)
  | .object _ fields =>
    nameSetUnions (fields.map (fun (_, ret, stmt) => (freeVarsStatement stmt).erase ret))
  | .mu _ name stmt => (freeVarsStatement stmt).erase name

partial def freeVarsConsumer : Consumer → Std.TreeSet Name
  | .label _ name => sing name
  | .apply _ ps ks =>
    (nameSetUnions (ps.map freeVarsProducer)).union (Std.TreeSet.ofList ks)
  | .project _ _ k => sing k
  | .«then» _ name stmt => (freeVarsStatement stmt).erase name
  | .finish _ => emptySet
  | .select _ branches => nameSetUnions (branches.map freeVarsBranch)

partial def freeVarsBranch : Branch → Std.TreeSet Name
  | .branch _ pat stmt => (freeVarsStatement stmt).diff (patternVars pat)

end

/-! ## Escaping names -/

mutual

partial def escapingNamesStatement : Statement → Std.TreeSet Name
  | .cut p _ => escapingNamesProducer p
  | .join _ _ consumer stmt =>
    (escapingNamesConsumer consumer).union (escapingNamesStatement stmt)
  | .primitive _ _ ps _ => nameSetUnions (ps.map escapingNamesProducer)
  | .invoke _ _ k => sing k
  | .externalCall _ _ ps _ => nameSetUnions (ps.map escapingNamesProducer)
  | .binOp _ _ lhs rhs _ => (escapingNamesProducer lhs).union (escapingNamesProducer rhs)
  | .ifz _ cond t e =>
    (escapingNamesProducer cond).union
      ((escapingNamesStatement t).union (escapingNamesStatement e))

-- `Lambda`/`Object`/`Mu` are nested-closure-body producers: an
-- escaping name of the enclosing statement is exactly a free variable of
-- theirs, so those cases delegate to `freeVarsProducer`.
partial def escapingNamesProducer : Producer → Std.TreeSet Name
  | .var _ _ => emptySet
  | .literal _ _ => emptySet
  | .construct _ _ ps ks =>
    (nameSetUnions (ps.map escapingNamesProducer)).union (Std.TreeSet.ofList ks)
  | .lambda r names stmt => freeVarsProducer (.lambda r names stmt)
  | .object r fields => freeVarsProducer (.object r fields)
  | .mu r name stmt => freeVarsProducer (.mu r name stmt)

partial def escapingNamesConsumer : Consumer → Std.TreeSet Name
  | .label _ _ => emptySet
  | .apply _ ps ks =>
    (nameSetUnions (ps.map escapingNamesProducer)).union (Std.TreeSet.ofList ks)
  | .project _ _ k => sing k
  | .«then» _ _ stmt => escapingNamesStatement stmt
  | .finish _ => emptySet
  | .select _ branches => nameSetUnions (branches.map escapingNamesBranch)

partial def escapingNamesBranch : Branch → Std.TreeSet Name
  | .branch _ _ stmt => escapingNamesStatement stmt

end

/-! ## Collecting joins -/

mutual

partial def collectJoins : Statement → List (Name × Consumer)
  | .cut p _ => collectJoinsProducer p
  | .join _ name consumer stmt =>
    (name, consumer) :: (collectJoinsConsumer consumer ++ collectJoins stmt)
  | .primitive _ _ ps _ => ps.flatMap collectJoinsProducer
  | .invoke _ _ _ => []
  | .externalCall _ _ ps _ => ps.flatMap collectJoinsProducer
  | .binOp _ _ lhs rhs _ => collectJoinsProducer lhs ++ collectJoinsProducer rhs
  | .ifz _ cond t e => collectJoinsProducer cond ++ collectJoins t ++ collectJoins e

partial def collectJoinsProducer : Producer → List (Name × Consumer)
  | .var _ _ => []
  | .literal _ _ => []
  | .construct _ _ ps _ => ps.flatMap collectJoinsProducer
  | .lambda _ _ _ => []
  | .object _ _ => []
  | .mu _ _ _ => []

partial def collectJoinsConsumer : Consumer → List (Name × Consumer)
  | .label _ _ => []
  | .apply _ ps _ => ps.flatMap collectJoinsProducer
  | .project _ _ _ => []
  | .«then» _ _ stmt => collectJoins stmt
  | .finish _ => []
  | .select _ branches => branches.flatMap (fun | .branch _ _ stmt => collectJoins stmt)

end

/-! ## Direct-escaping classification -/

mutual

partial def initialClassifyJoinsWithEscaping : Statement → OwnershipMap × Std.TreeSet Name
  | .cut p _ => (initialClassifyJoinsProducer p, escapingNamesProducer p)
  | .join _ name consumer stmt =>
    let (m, esc) := initialClassifyJoinsWithEscaping stmt
    let ownership := if esc.contains name then Ownership.Escaping else Ownership.Local
    let (cm, cesc) := initialClassifyJoinsConsumerWithEscaping consumer
    ((mapUnion cm m).insert name ownership, cesc.union esc)
  | .primitive _ _ ps _ =>
    (mapUnions (ps.map initialClassifyJoinsProducer), nameSetUnions (ps.map escapingNamesProducer))
  | .invoke _ _ k => ({}, sing k)
  | .externalCall _ _ ps _ =>
    (mapUnions (ps.map initialClassifyJoinsProducer), nameSetUnions (ps.map escapingNamesProducer))
  | .binOp _ _ lhs rhs _ =>
    (mapUnion (initialClassifyJoinsProducer lhs) (initialClassifyJoinsProducer rhs),
      (escapingNamesProducer lhs).union (escapingNamesProducer rhs))
  | .ifz _ cond t e =>
    let (mt, escT) := initialClassifyJoinsWithEscaping t
    let (me, escE) := initialClassifyJoinsWithEscaping e
    (mapUnions [initialClassifyJoinsProducer cond, mt, me],
      nameSetUnions [escapingNamesProducer cond, escT, escE])

partial def initialClassifyJoinsProducer : Producer → OwnershipMap
  | .var _ _ => {}
  | .literal _ _ => {}
  | .construct _ _ ps _ => mapUnions (ps.map initialClassifyJoinsProducer)
  | .lambda _ _ _ => {}
  | .object _ _ => {}
  | .mu _ _ _ => {}

partial def initialClassifyJoinsConsumerWithEscaping : Consumer → OwnershipMap × Std.TreeSet Name
  | .label _ _ => ({}, {})
  | .apply _ ps ks =>
    (mapUnions (ps.map initialClassifyJoinsProducer),
      (nameSetUnions (ps.map escapingNamesProducer)).union (Std.TreeSet.ofList ks))
  | .project _ _ k => ({}, sing k)
  | .«then» _ _ stmt => initialClassifyJoinsWithEscaping stmt
  | .finish _ => ({}, {})
  | .select _ branches =>
    let results := branches.map (fun | .branch _ _ stmt => initialClassifyJoinsWithEscaping stmt)
    (mapUnions (results.map Prod.fst), nameSetUnions (results.map Prod.snd))

end

def initialClassifyJoins (s : Statement) : OwnershipMap :=
  (initialClassifyJoinsWithEscaping s).1

def initialClassifyJoinsConsumer (c : Consumer) : OwnershipMap :=
  (initialClassifyJoinsConsumerWithEscaping c).1

/-- Repeatedly promote any `Local` join referenced as a free variable of an
`Escaping` join's consumer, until no more changes. -/
partial def promoteEscapingCaptures
    (consumers : Std.TreeMap Name Consumer) (ownership : OwnershipMap) : OwnershipMap :=
  let escapingConsumers :=
    consumers.toList.filterMap (fun (n, c) =>
      if ownership.get? n == some Ownership.Escaping then some c else none)
  let referenced := nameSetUnions (escapingConsumers.map freeVarsConsumer)
  let toPromote := referenced.filter (fun n => ownership.get? n == some Ownership.Local)
  if toPromote.isEmpty then ownership
  else
    let promoted := toPromote.foldl (fun acc n => acc.insert n Ownership.Escaping) {}
    promoteEscapingCaptures consumers (mapUnion promoted ownership)

def classifyJoins (stmt : Statement) : OwnershipMap :=
  promoteEscapingCaptures (Std.TreeMap.ofList (collectJoins stmt)) (initialClassifyJoins stmt)

def classifyJoinsConsumer (consumer : Consumer) : OwnershipMap :=
  promoteEscapingCaptures
    (Std.TreeMap.ofList (collectJoinsConsumer consumer)) (initialClassifyJoinsConsumer consumer)

end Malgo.Sequent.Core.Escape
