import Std.Data.TreeMap

/-! Order-faithful port of containers' `Data.Graph.stronglyConnComp`
(Kosaraju: DFS on the transposed graph, reverse postorder, DFS again),
specialized to what `makeBindGroup` needs. The resulting group order
(reverse topological — dependencies first) and the order inside each
cyclic group feed uniq assignment downstream, so every detail mirrors
containers: nodes sorted by key, unknown keys dropped, transposed
adjacency built by prepending (reversed), preorder flattening. -/

namespace Malgo.Data

private inductive Tree where
  | node (v : Nat) (children : List Tree)

/-- containers' `chop`: DFS over roots, skipping visited vertices. -/
private partial def chop (adj : Array (Array Nat)) :
    List Nat → Array Bool → (List Tree × Array Bool)
  | [], visited => ([], visited)
  | v :: vs, visited =>
    if visited[v]! then
      chop adj vs visited
    else
      let visited := visited.set! v true
      let (children, visited) := chop adj adj[v]!.toList visited
      let (rest, visited) := chop adj vs visited
      (.node v children :: rest, visited)

private def dfs (adj : Array (Array Nat)) (roots : List Nat) : List Tree :=
  (chop adj roots (Array.replicate adj.size false)).1

/-- containers' `transposeG` via `buildG`: edges enumerated in vertex
order, each prepended — adjacency lists come out reversed. -/
private def transpose (adj : Array (Array Nat)) : Array (Array Nat) := Id.run do
  let mut t : Array (Array Nat) := Array.replicate adj.size #[]
  for v in [0:adj.size] do
    for w in adj[v]! do
      t := t.set! w (#[v] ++ t[w]!)
  return t

/-- containers' `postorder`: children first, then the node. -/
private partial def postorderTree (t : Tree) (acc : List Nat) : List Nat :=
  match t with
  | .node v ts => ts.foldr postorderTree (v :: acc)

/-- containers' `flattenSCC` shape (`dec`): preorder. -/
private partial def flattenTree (t : Tree) (acc : List Nat) : List Nat :=
  match t with
  | .node v ts => v :: ts.foldr flattenTree acc

/-- `stronglyConnComp` + `flattenSCC` over `(key, out-edge keys)` nodes.
Groups are reverse topologically sorted: dependencies before dependents. -/
def sccGroups [Ord κ] (nodes : List (κ × List κ)) : List (List κ) :=
  let sorted := nodes.mergeSort fun a b => compare a.1 b.1 != .gt
  let keys : Array κ := (sorted.map (·.1)).toArray
  let keyIdx : Std.TreeMap κ Nat :=
    keys.toList.zipIdx.foldl (init := {}) fun m (k, i) => m.insert k i
  let adj : Array (Array Nat) :=
    (sorted.map fun (_, ks) => (ks.filterMap keyIdx.get?).toArray).toArray
  let post := (dfs (transpose adj) (List.range adj.size)).foldr postorderTree []
  let forest := dfs adj post.reverse
  forest.map fun t => (flattenTree t []).filterMap (keys[·]?)

-- Group order and in-cycle order pinned against GHC containers
-- (`ghc -e 'map flattenSCC (stronglyConnComp …)'`).
#guard sccGroups [("c", ["a"]), ("a", ["b"]), ("b", ["a"])] == [["a", "b"], ["c"]]
#guard sccGroups [("main", ["f", "g"]), ("f", ["g", "h"]), ("g", ["f"]), ("h", [])]
  == [["h"], ["f", "g"], ["main"]]
#guard sccGroups ([] : List (String × List String)) == []

end Malgo.Data
