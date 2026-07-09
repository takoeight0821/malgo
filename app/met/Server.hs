-- | The MET (M-exp-Tracer) web UI: a self-contained HTML front end over a
-- pre-computed '[Stage]'. Handlers are pure functions over that list — the
-- Malgo compiler effects only ever run once, before 'app' is constructed
-- (see "Main").
module Server (app) where

import Data.Text qualified as T
import Lucid hiding (for_)
import Malgo.Debug.DiffView (DiffLine (..), LineTag (..), SideBySideRow (..), Span (..), sideBySide, unifiedDiff)
import Malgo.Debug.Pipeline (Stage (..))
import Malgo.Prelude
import Servant
import Servant.HTML.Lucid (HTML)

type TraceAPI =
  Get '[HTML] (Html ())
    :<|> "stage" :> Capture "index" Int :> QueryParam "view" Text :> Get '[HTML] (Html ())

app :: [Stage] -> Application
app stages = serve (Proxy @TraceAPI) (server stages)

server :: [Stage] -> Server TraceAPI
server stages = homeHandler :<|> stageHandler
  where
    homeHandler = pure (pageLayout "MET" (homeBody stages))
    stageHandler i view = case (stageAt stages (i - 1), stageAt stages i) of
      (Just before, Just after) ->
        pure (pageLayout (before.name <> " -> " <> after.name) (transitionBody stages i before after view))
      _ -> throwError err404 {errBody = "No such stage transition"}

stageAt :: [Stage] -> Int -> Maybe Stage
stageAt stages i
  | i >= 0 && i < length stages = Just (stages !! i)
  | otherwise = Nothing

homeBody :: [Stage] -> Html ()
homeBody stages = do
  h1_ "MET — M-exp-Tracer"
  p_ "Every pipeline stage traced for this run, Parse through the Zig backend's Reuse pass:"
  nav_
    $ ul_
    $ for_ (zip [1 :: Int ..] (drop 1 stages))
    $ \(i, after) -> do
      let before = stages !! (i - 1)
      li_ $ a_ [href_ (stagePath i)] (toHtml (before.name <> " -> " <> after.name))

transitionBody :: [Stage] -> Int -> Stage -> Stage -> Maybe Text -> Html ()
transitionBody stages i before after view = do
  h1_ (toHtml (before.name <> " -> " <> after.name))
  p_ $ do
    a_ [href_ "/"] "Home"
    " | "
    navLink (i - 1) "« Prev"
    " | "
    navLink (i + 1) "Next »"
  p_ $ do
    let isDiff = view == Just "diff"
        sideLink = a_ [href_ (stagePath i)] "Side-by-side"
        diffLink = a_ [href_ (stagePath i <> "?view=diff")] "Diff patch"
    if isDiff then sideLink else strong_ sideLink
    " | "
    if isDiff then strong_ diffLink else diffLink
  if view == Just "diff"
    then diffView before after
    else sideBySideView before after
  where
    navLink target label
      | target >= 1 && target <= length stages - 1 = a_ [href_ (stagePath target)] label
      | otherwise = span_ [class_ "disabled"] label

stagePath :: Int -> Text
stagePath i = "/stage/" <> T.pack (show i)

diffView :: Stage -> Stage -> Html ()
diffView before after =
  pre_ [class_ "diff"] $ for_ (unifiedDiff before.rendered after.rendered) diffLineHtml
  where
    diffLineHtml :: DiffLine -> Html ()
    diffLineHtml (DiffLine tag spans) = div_ [class_ ("line " <> tagClass tag)] (renderSpans spans)
    tagClass Unchanged = "unchanged"
    tagClass Added = "added"
    tagClass Removed = "removed"

sideBySideView :: Stage -> Stage -> Html ()
sideBySideView before after =
  table_ [class_ "side-by-side"] $ for_ (sideBySide before.rendered after.rendered) rowHtml
  where
    rowHtml :: SideBySideRow -> Html ()
    rowHtml SideBySideRow {leftTag, leftSpans, rightTag, rightSpans} =
      tr_ $ do
        td_ [class_ (cellClass leftTag)] (maybe (pure ()) renderSpans leftSpans)
        td_ [class_ (cellClass rightTag)] (maybe (pure ()) renderSpans rightSpans)
    cellClass Nothing = "cell empty"
    cellClass (Just Unchanged) = "cell"
    cellClass (Just Added) = "cell added"
    cellClass (Just Removed) = "cell removed"

-- | Renders word-level spans within one line: 'Unchanged' runs are plain
-- text (they inherit the line's own light background), 'Added'\/'Removed'
-- runs get a stronger highlight so the specific token that changed stands
-- out against an otherwise-identical line.
renderSpans :: [Span] -> Html ()
renderSpans spans = for_ spans spanHtml
  where
    spanHtml (Span Unchanged t) = toHtml t
    spanHtml (Span Added t) = span_ [class_ "sub-added"] (toHtml t)
    spanHtml (Span Removed t) = span_ [class_ "sub-removed"] (toHtml t)

pageLayout :: Text -> Html () -> Html ()
pageLayout title body = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    title_ (toHtml title)
    style_ css
  body_
    $ div_ [class_ "layout"]
    $ do
      main_ [class_ "content"] body
      helpSidebar

-- | (group name, [(keyword\/symbol, meaning)]) — the notation
-- 'Malgo.Debug.PrettyIR' invents for each stage family isn't real Malgo
-- syntax, so this is the legend explaining what each token means.
helpSections :: [(Text, [(Text, Text)])]
helpSections =
  [ ( "Surface syntax (Parse / Rename)",
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
        ("let x = e / with x = e", "do-notation binding")
      ]
    ),
    ( "Core IR (ToFun \8594 ToCore \8594 Flat \8594 Join, sequent calculus)",
      [ ("p ~ c", "cut: run producer p, hand the value to consumer c"),
        ("then x -> { s }", "consumer that binds the value to x, then runs s"),
        ("join j = c in s", "names consumer c as j (a join point/basic-block label) so s can jump to it by name"),
        ("select { pat -> s | .. }", "pattern-match consumer"),
        (".field -> c", "record field projection consumer"),
        (".ctor(args) -> c", "codata destructor consumer"),
        ("finish", "the top-level \"program is done\" consumer"),
        ("mu k. { s }", "binds the current consumer to k (classical control, like call/cc)"),
        ("do x . { s }", "sequences a nested statement, naming its result x (Core.Full only; gone after FlatPass)"),
        ("cocase { .m(args) -> s | .. }", "codata/object literal"),
        ("invoke f", "tail-call a top-level definition"),
        ("extern f(args)", "foreign call"),
        ("#op(args)", "primitive operation"),
        ("if0 p then {..} else {..}", "branch on p == 0")
      ]
    ),
    ( "Zig backend ANF IR (ClosureConv \8594 Peephole \8594 Perceus \8594 Reuse)",
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
        (".kind == / .tag == / == lit", "pattern-match guard tests")
      ]
    ),
    ( "Shared",
      [ ("name#N", "an internal (module-local) identifier, disambiguated by a unique number"),
        ("name$N", "a compiler-generated temporary identifier"),
        ("Tag(args)", "data constructor application (Tuple for an anonymous tuple)")
      ]
    )
  ]

helpSidebar :: Html ()
helpSidebar =
  aside_ [class_ "help"] $ do
    h2_ "Notation legend"
    p_ "This is MET's own best-effort ASCII notation, not real Malgo syntax."
    for_ helpSections \(title, entries) ->
      details_ $ do
        summary_ (toHtml title)
        dl_ $ for_ entries \(kw, desc) -> do
          dt_ (toHtml kw)
          dd_ (toHtml desc)

css :: Text
css =
  T.unlines
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
      "table.side-by-side td.empty { background: #f6f8fa; }"
    ]
