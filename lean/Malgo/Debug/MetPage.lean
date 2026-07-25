import Malgo.Debug.DiffView
import Malgo.Debug.Pipeline

/-! Port of `app/met/{Main,Server}.hs` (MET — the M-exp-Tracer): renders the
whole pipeline trace of one `.mlg` file. Per the plan's M8 decision, this is
NOT a web server (Lean networking is still `Std.Internal`) — the Haskell
`servant` app is already pure over a precomputed `[Stage]` (every handler is
a function from `[Stage]` to `Html ()`, with no further compiler effects),
so nothing is lost by rendering it all into ONE self-contained static HTML
page instead: every stage transition's side-by-side/diff views, all
present at once, with a few lines of inline JS to toggle which view is
visible per transition (matching the original's `?view=diff` query-param
toggle) instead of Haskell's per-transition server route (`/stage/:i`) and
prev/next links (superseded here by in-page `#stage-N` anchors — an
ordinary browser back/forward-free scroll replaces the page-to-page
navigation entirely). -/

namespace Malgo.Debug.MetPage

open Malgo.Debug.DiffView
open Malgo.Debug.Pipeline (Stage)

/-- Minimal HTML entity escaping for arbitrary source-derived text (a
`.mlg` file's own content, or its path, can contain any of `&<>"'`). -/
def escapeHtml (s : String) : String :=
  String.join (s.toList.map fun c => match c with
    | '&' => "&amp;"
    | '<' => "&lt;"
    | '>' => "&gt;"
    | '"' => "&quot;"
    | '\'' => "&#39;"
    | _ => String.singleton c)

private def tagClass : LineTag → String
  | .unchanged => "unchanged"
  | .added => "added"
  | .removed => "removed"

/-- Renders word-level spans within one line: `Unchanged` runs are plain
text (they inherit the line's own light background), `Added`/`Removed`
runs get a stronger highlight so the specific token that changed stands
out against an otherwise-identical line. -/
private def renderSpans (spans : List Span) : String :=
  String.join (spans.map fun s => match s.tag with
    | .unchanged => escapeHtml s.text
    | .added => s!"<span class=\"sub-added\">{escapeHtml s.text}</span>"
    | .removed => s!"<span class=\"sub-removed\">{escapeHtml s.text}</span>")

private def diffLineHtml (dl : DiffLine) : String :=
  s!"<div class=\"line {tagClass dl.lineTag}\">{renderSpans dl.spans}</div>"

private def diffViewHtml (before after : String) : String :=
  s!"<pre class=\"diff\">{String.join ((unifiedDiff before after).map diffLineHtml)}</pre>"

private def cellClass : Option LineTag → String
  | none => "cell empty"
  | some .unchanged => "cell"
  | some .added => "cell added"
  | some .removed => "cell removed"

private def rowHtml (row : SideBySideRow) : String :=
  let leftHtml := match row.leftSpans with | some sp => renderSpans sp | none => ""
  let rightHtml := match row.rightSpans with | some sp => renderSpans sp | none => ""
  s!"<tr><td class=\"{cellClass row.leftTag}\">{leftHtml}</td><td class=\"{cellClass row.rightTag}\">{rightHtml}</td></tr>"

private def sideBySideViewHtml (before after : String) : String :=
  s!"<table class=\"side-by-side\">{String.join ((sideBySide before after).map rowHtml)}</table>"

/-- One stage transition: a heading, a side-by-side/diff-patch toggle, and
both views (only one visible at a time, via `metToggleView`/the `hidden`
class — see `script`). -/
private def stageSectionHtml (i : Nat) (before after : Stage) : String :=
  let label := escapeHtml before.name ++ " -&gt; " ++ escapeHtml after.name
  s!"<section id=\"stage-{i}\">
<h2>{label}</h2>
<p>
<button type=\"button\" onclick=\"metToggleView({i}, 'side')\">Side-by-side</button>
 | <button type=\"button\" onclick=\"metToggleView({i}, 'diff')\">Diff patch</button>
</p>
<div id=\"stage-{i}-side\" class=\"view\">{sideBySideViewHtml before.rendered after.rendered}</div>
<div id=\"stage-{i}-diff\" class=\"view hidden\">{diffViewHtml before.rendered after.rendered}</div>
</section>"

/-- One `<li>` per stage transition, linking to its `#stage-N` section. -/
private def navHtml (stages : List Stage) : String :=
  let items := (List.range (stages.length - 1)).filterMap fun idx =>
    let i := idx + 1
    match stages[idx]?, stages[i]? with
    | some before, some after =>
      some s!"<li><a href=\"#stage-{i}\">{escapeHtml before.name} -&gt; {escapeHtml after.name}</a></li>"
    | _, _ => none
  s!"<nav><ul>\n{String.intercalate "\n" items}\n</ul></nav>"

/-- Every stage transition's section, concatenated in pipeline order. -/
private def sectionsHtml (stages : List Stage) : String :=
  String.intercalate "\n" <| (List.range (stages.length - 1)).filterMap fun idx =>
    let i := idx + 1
    match stages[idx]?, stages[i]? with
    | some before, some after => some (stageSectionHtml i before after)
    | _, _ => none

/-- (group name, [(keyword/symbol, meaning)]) — the notation
`Malgo.Debug.PrettyIR` invents for each stage family isn't real Malgo
syntax, so this is the legend explaining what each token means. Verbatim
port of Haskell's `helpSections`. -/
private def helpSections : List (String × List (String × String)) :=
  [ ("Surface syntax (Parse / Rename)",
      [ ("def f = e", "value/function definition"),
        ("sig f : ty", "type signature"),
        ("data T a = C(ty..) | ..", "data type declaration"),
        ("type T a = ty", "type synonym"),
        ("foreign f : ty", "builtin/foreign declaration"),
        ("import M (..)", "module import"),
        ("infixl / infixr / infix", "operator fixity declaration"),
        ("fn { pat -> e | .. }", "multi-clause function literal"),
        ("codata { .copat -> e }", "codata (object) literal"),
        ("label x . e / goto v l", "first-class label / non-local jump"),
        ("let x = e / with x = e", "do-notation binding") ]),
    ("Core IR (ToFun → ToCore → Flat → Join, sequent calculus)",
      [ ("p ~ c", "cut: run producer p, hand the value to consumer c"),
        ("then x -> { s }", "consumer that binds the value to x, then runs s"),
        ("join j = c in s", "names consumer c as j (a join point/basic-block label) so s can jump to it by name"),
        ("select { pat -> s | .. }", "pattern-match consumer"),
        (".field -> c", "record field projection consumer"),
        ("finish", "the top-level \"program is done\" consumer"),
        ("mu k. { s }", "binds the current consumer to k (classical control, like call/cc)"),
        ("do x . { s }", "sequences a nested statement, naming its result x (Core.Full only; gone after FlatPass)"),
        ("invoke f", "tail-call a top-level definition"),
        ("extern f(args)", "foreign call"),
        ("#op(args)", "primitive operation"),
        ("if0 p then {..} else {..}", "branch on p == 0") ]),
    ("Zig backend ANF IR (ClosureConv → Peephole → Perceus → Reuse)",
      [ ("toplevel / closure / field fn", "function kind — closure/field fns receive a self param, toplevel doesn't"),
        ("let x = expr", "bind a fresh value"),
        ("dup x / drop x", "Perceus reference counting: dup adds a reference, drop releases one"),
        ("dropReuse tok = x / n", "Reuse pass: drop x's reference, remember it as an n-field reuse token"),
        ("reuse tok Tag(args)", "rebuild a struct in place via tok's token when possible"),
        ("return ..", "a terminator: the function's tail call / return value"),
        ("if guard then {..} else {..}", "branch (pattern-match compilation)"),
        ("panic \"msg\"", "unreachable / no matching branch"),
        ("x.cap[i]", "read the i-th capture out of a closure/self"),
        ("x!field", "force a lazy record field"),
        (".kind == / .tag == / == lit", "pattern-match guard tests") ]),
    ("Shared",
      [ ("name#N", "an internal (module-local) identifier, disambiguated by a unique number"),
        ("name$N", "a compiler-generated temporary identifier"),
        ("Tag(args)", "data constructor application (Tuple for an anonymous tuple)") ]) ]

private def helpSidebarHtml : String :=
  let sections := String.join (helpSections.map fun (title, entries) =>
    let dl := String.join (entries.map fun (kw, desc) =>
      s!"<dt>{escapeHtml kw}</dt><dd>{escapeHtml desc}</dd>")
    s!"<details><summary>{escapeHtml title}</summary><dl>{dl}</dl></details>")
  s!"<aside class=\"help\">
<h2>Notation legend</h2>
<p>This is MET's own best-effort ASCII notation, not real Malgo syntax.</p>
{sections}
</aside>"

private def css : String :=
  String.intercalate "\n"
    [ "body { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; margin: 2rem; }",
      ".layout { display: flex; align-items: flex-start; gap: 1.5rem; }",
      ".layout .content { flex: 1; min-width: 0; }",
      "aside.help { flex: 0 0 300px; width: 300px; position: sticky; top: 1rem; max-height: 95vh; overflow-y: auto; font-size: 0.8rem; border-left: 1px solid #ddd; padding-left: 1rem; }",
      "aside.help h2 { font-size: 0.95rem; margin-top: 0; }",
      "aside.help summary { cursor: pointer; font-weight: bold; margin: 0.5rem 0; }",
      "aside.help dl { margin: 0.25rem 0 0.75rem 0; }",
      "aside.help dt { font-family: ui-monospace, monospace; font-weight: bold; margin-top: 0.4rem; }",
      "aside.help dd { margin: 0.1rem 0 0 0.5rem; color: #444; }",
      "nav ul { padding-left: 1.2rem; }",
      ".disabled { color: #999; }",
      ".hidden { display: none; }",
      "pre.diff { white-space: pre-wrap; border: 1px solid #ddd; padding: 0.5rem; }",
      ".diff .line { white-space: pre-wrap; }",
      ".diff .line.added { background: #e6ffed; color: #22863a; }",
      ".diff .line.removed { background: #ffeef0; color: #b31d28; }",
      ".sub-added { background: #acf2bd; }",
      ".sub-removed { background: #fdb8c0; }",
      "table.side-by-side { width: 100%; border-collapse: collapse; table-layout: fixed; }",
      "table.side-by-side td { width: 50%; vertical-align: top; padding: 0 0.5rem; white-space: pre-wrap; word-break: break-word; font-family: ui-monospace, monospace; border: 1px solid #ddd; }",
      "table.side-by-side td.added { background: #e6ffed; }",
      "table.side-by-side td.removed { background: #ffeef0; }",
      "table.side-by-side td.empty { background: #f6f8fa; }" ]

/-- Toggles which of a transition's two views (`side`/`diff`) is visible,
replacing Haskell's server-side `?view=diff` query param + page reload. -/
private def script : String :=
  "function metToggleView(i, which) {\n" ++
  "  document.getElementById('stage-' + i + '-side').classList.toggle('hidden', which !== 'side');\n" ++
  "  document.getElementById('stage-' + i + '-diff').classList.toggle('hidden', which !== 'diff');\n" ++
  "}"

/-- Render the whole trace of `srcPath` (already-run `stages`) as one
self-contained HTML document. -/
def renderPage (srcPath : String) (stages : List Stage) : String :=
  s!"<!doctype html>
<html>
<head>
<meta charset=\"utf-8\">
<title>MET — {escapeHtml srcPath}</title>
<style>{css}</style>
</head>
<body>
<div class=\"layout\">
<main class=\"content\">
<h1>MET — M-exp-Tracer</h1>
<p>Every pipeline stage traced for {escapeHtml srcPath}, Parse through the Zig backend's Reuse pass:</p>
{navHtml stages}
{sectionsHtml stages}
</main>
{helpSidebarHtml}
</div>
<script>{script}</script>
</body>
</html>"

end Malgo.Debug.MetPage
