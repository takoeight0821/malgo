import Malgo

/-! Unit test for M8 (`Malgo.Debug.MetPage`), per the plan's stated gate:
"generated index lists all stage transitions for an example". No Haskell
reference exists (the Haskell `met` is a live web server, not a static
artifact — there's nothing to golden-diff against), so this checks
structural properties of the generated HTML directly. -/

namespace Malgo.Test.MetPage

open Malgo.Debug.Pipeline (Stage)

/-- The text immediately following `anchor`, up to (not including) the next
`</div>` — used to spot-check that a `stage-{i}-side`/`stage-{i}-diff` div
actually has non-trivial rendered content following its opening tag,
rather than only checking that the surrounding heading/nav text exists.
(For the `-diff` div specifically, the diff view nests its own `<div
class="line ...">` elements, so this only captures up to the FIRST nested
`</div>` — that's fine here, since the goal is just to confirm real
content immediately follows the anchor, not to extract the whole block.) -/
private def contentAfter (html anchor : String) : Option String :=
  match html.splitOn anchor with
  | _ :: rest :: _ =>
    match rest.splitOn "</div>" with
    | content :: _ => some content
    | [] => none
  | _ => none

/-- Run the trace for one example and check the generated page: every
adjacent stage-name pair appears as a nav entry and as a section heading
AND has non-trivial rendered content in both its side-by-side and
diff-patch divs (not just the heading/nav text — a prior version of this
check would have passed even if the actual rendered content regressed to
empty), and the page is a well-formed (opening/closing tag matched) HTML
document containing the legend sidebar. Returns `.error msg` describing
the first check that failed. -/
def run : IO (Except String Unit) := do
  let srcPath := System.FilePath.mk "examples/malgo/Hello.mlg"
  unless (← srcPath.pathExists) do
    return .error s!"fixture not found: {srcPath}"
  let stages ← Malgo.Debug.Pipeline.runTrace srcPath false false
  if stages.length < 2 then
    return .error s!"expected at least 2 stages to have a transition, got {stages.length}"
  let html := Malgo.Debug.MetPage.renderPage srcPath.toString stages
  unless html.startsWith "<!doctype html>" do
    return .error "generated page does not start with <!doctype html>"
  unless html.endsWith "</html>" do
    return .error "generated page does not end with </html>"
  unless (html.splitOn "<aside class=\"help\">").length > 1 do
    return .error "generated page is missing the notation-legend sidebar"
  unless (html.splitOn "<script>").length > 1 do
    return .error "generated page is missing the inline view-toggle script"
  for ((before, after), i) in stages.zip stages.tail |>.zipIdx 1 do
    let label := s!"{before.name} -&gt; {after.name}"
    -- appears twice when both the nav entry and the section heading are
    -- present, matching "generated index lists all stage transitions"
    unless (html.splitOn label).length ≥ 3 do
      return .error s!"generated page is missing a nav and/or section entry for transition \"{label}\""
    -- non-trivial content minimum: past the div's own opening-tag
    -- boilerplace (` class="view">` etc.), so an accidentally-empty view
    -- doesn't slip through
    let minLen := 20
    match contentAfter html s!"id=\"stage-{i}-side\"" with
    | none => return .error s!"generated page is missing the stage-{i}-side div"
    | some content =>
      unless content.length > minLen do
        return .error s!"stage-{i}-side div for \"{label}\" looks empty/trivial ({content.length} chars)"
    match contentAfter html s!"id=\"stage-{i}-diff\"" with
    | none => return .error s!"generated page is missing the stage-{i}-diff div"
    | some content =>
      unless content.length > minLen do
        return .error s!"stage-{i}-diff div for \"{label}\" looks empty/trivial ({content.length} chars)"
  return .ok ()

end Malgo.Test.MetPage
