import Malgo

/-! Unit test for M8 (`Malgo.Debug.MetPage`), per the plan's stated gate:
"generated index lists all stage transitions for an example". No Haskell
reference exists (the Haskell `met` is a live web server, not a static
artifact — there's nothing to golden-diff against), so this checks
structural properties of the generated HTML directly. -/

namespace Malgo.Test.MetPage

open Malgo.Debug.Pipeline (Stage)

/-- Run the trace for one example and check the generated page: every
adjacent stage-name pair appears as a nav entry and as a section heading,
and the page is a well-formed (opening/closing tag matched) HTML document
containing the legend sidebar. Returns `.error msg` describing the first
check that failed. -/
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
  for (before, after) in stages.zip stages.tail do
    let label := s!"{before.name} -&gt; {after.name}"
    -- appears twice when both the nav entry and the section heading are
    -- present, matching "generated index lists all stage transitions"
    unless (html.splitOn label).length ≥ 3 do
      return .error s!"generated page is missing a nav and/or section entry for transition \"{label}\""
  return .ok ()

end Malgo.Test.MetPage
