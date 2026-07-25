-- | Renders each stage of the compilation pipeline in Malgo-ish syntax, for
-- the MET (M-exp-Tracer) debug tool ("app/met") and its golden tests. This
-- is deliberately a best-effort, non-round-trippable rendering: the goal is
-- a human-readable text a developer can diff between passes, not a printer
-- that could re-parse.
--
-- The sequent-calculus IRs (Core.Full\/Flat\/Join) have no direct Malgo
-- surface-syntax equivalent, so they use a light ASCII notation:
-- @producer ~ consumer@ for a cut, @.field -> k@ for a projection\/
-- destructor continuation, and @{ ... }@ for statement bodies.
--
-- Every nested body goes through 'block'\/'blockLines'\/'renderArgs' rather
-- than raw 'braces'\/'parens', so long definitions (e.g. after closure
-- conversion, where locally-bound joins get inlined at every use site) wrap
-- onto indented lines instead of rendering as one huge line — which also
-- keeps line-based diffing (see "Malgo.Debug.DiffView") meaningful.
module Malgo.Debug.PrettyIR
  ( renderParsedModule,
    renderBindGroup,
    renderFun,
    renderCoreFull,
    renderFlat,
    renderJoin,
    renderZigIr,
  )
where

import Data.Map.Strict qualified as Map
import Malgo.Backend.Zig.Ir qualified as Ir
import Malgo.Id (Id (..), IdSort (..))
import Malgo.Module (ModuleName)
import Malgo.Prelude hiding (All)
import Malgo.Sequent.Core.Flat qualified as Flat
import Malgo.Sequent.Core.Full qualified as Full
import Malgo.Sequent.Core.Join qualified as Join
import Malgo.Sequent.Fun (Literal (..), Pattern (..), Tag (..))
import Malgo.Sequent.Fun qualified as Fun
import Malgo.Syntax qualified as Syntax
import Malgo.Syntax.Extension
import Prettyprinter
  ( concatWith,
    dquotes,
    encloseSep,
    flatAlt,
    group,
    hardline,
    hsep,
    lbrace,
    line,
    list,
    nest,
    parens,
    punctuate,
    rbrace,
    sep,
    squotes,
    tupled,
    (<+>),
  )

-- * Shared helpers (Name\/Tag\/Literal are the same types across every IR)

-- | Drops the module qualifier: a trace is always a single unlinked module,
-- so every name shares the same qualifier and repeating it is just noise.
renderName :: Id -> Doc ann
renderName Id {name, sort = External} = pretty name
renderName Id {name, sort = Internal u} = pretty name <> "#" <> pretty u
renderName Id {name, sort = Temporal u} = pretty name <> "$" <> pretty u

-- | Surface-syntax identifiers are 'Text' at the Parse phase and resolved
-- 'Id's after Rename. Dispatching through this class means Rename-phase
-- output uses the same plain @name$uniq@\/@name#uniq@ style as every other
-- IR ('renderName') instead of 'Id'\'s own 'Pretty' instance, which spells
-- out the full module-qualified bracket notation and reads poorly here.
class RenderId a where
  renderId :: a -> Doc ann

instance RenderId Text where
  renderId = pretty

instance RenderId Id where
  renderId = renderName

renderTag :: Tag -> Doc ann
renderTag Tuple = "Tuple"
renderTag (Tag t) = pretty t

renderLit :: Literal -> Doc ann
renderLit = \case
  Int32 n -> pretty n
  Int64 n -> pretty n <> "L"
  Float f -> pretty f <> "f"
  Double d -> pretty d
  Char c -> squotes (pretty c)
  String s -> dquotes (pretty s)

-- | Comma-separated items in parens: one line if it fits, else one item
-- per line (leading comma), indented.
renderArgs :: [Doc ann] -> Doc ann
renderArgs = tupled

-- | Like 'renderArgs', but brace-delimited (records\/objects).
braceList :: [Doc ann] -> Doc ann
braceList = group . encloseSep (flatAlt (lbrace <> " ") lbrace) (flatAlt (" " <> rbrace) rbrace) ", "

-- | A single nested statement\/body in braces, always expanded onto its
-- own indented line(s) — deliberately unconditional (no 'group'), so a
-- block's formatting is a pure function of its content and never flips
-- between one-line and expanded depending on how much of the line is
-- already used by its surroundings. That determinism also makes diffs
-- stable: an unrelated change elsewhere on the line can't reflow a block
-- that didn't itself change.
--
-- As a side effect, since a 'hardline' can never be flattened away by an
-- enclosing 'group', wrapping every @Then@\/@Join@ continuation's body in
-- 'block' forces every ancestor construct (@hangEq@'s @def ... =@,
-- @join ... = ... in ...@, etc.) that contains one to break too —
-- cascading real newlines through deeply chained statements instead of
-- collapsing the whole chain onto one line.
block :: Doc ann -> Doc ann
block content = "{" <> nest 2 (hardline <> content) <> hardline <> "}"

-- | A list of branches\/arms in braces, always one per line — collapsing
-- match arms onto one line hurts readability even when they would fit.
blockLines :: [Doc ann] -> Doc ann
blockLines [] = "{}"
blockLines docs = "{" <> nest 2 (hardline <> vsepHard docs) <> hardline <> "}"

-- | Like 'vsep', but never collapsed by an enclosing 'group'.
vsepHard :: [Doc ann] -> Doc ann
vsepHard = concatWith (\a b -> a <> hardline <> b)

-- | @prefix = body@: inline if it fits, else @body@ indented on the next line.
hangEq :: Doc ann -> Doc ann -> Doc ann
hangEq prefix body = group (prefix <+> "=" <> nest 2 (line <> body))

renderPattern :: Pattern -> Doc ann
renderPattern = \case
  PVar _ name -> renderName name
  PLiteral _ lit -> renderLit lit
  Destruct _ tag pats -> renderTag tag <> renderArgs (map renderPattern pats)
  Expand _ fields -> braceList [pretty k <+> "=" <+> renderPattern v | (k, v) <- Map.toList fields]

-- | One blank line between each top-level definition.
renderDefs :: [Doc ann] -> Text
renderDefs = render . concatWith (\a b -> a <> hardline <> hardline <> b)

-- * Fun IR

renderFun :: Fun.Program -> Text
renderFun Fun.Program {definitions} = renderDefs (map renderDef definitions)
  where
    renderDef (_, name, body) = hangEq ("def" <+> renderName name) (renderExpr body)

renderExpr :: Fun.Expr -> Doc ann
renderExpr = \case
  Fun.Var _ name -> renderName name
  Fun.Literal _ lit -> renderLit lit
  Fun.Construct _ tag args -> renderTag tag <> renderArgs (map renderExpr args)
  Fun.Let _ name value body ->
    group
      ( "let"
          <+> renderName name
          <+> "="
            <> nest 2 (line <> renderExpr value)
            <> line
            <> "in"
            <> nest 2 (line <> renderExpr body)
      )
  Fun.Lambda _ params body -> group ("\\" <> hsep (map renderName params) <+> "->" <> nest 2 (line <> renderExpr body))
  Fun.Object _ fields -> braceList [pretty k <+> "=" <+> renderExpr v | (k, v) <- Map.toList fields]
  Fun.Apply _ callee args -> renderExpr callee <> renderArgs (map renderExpr args)
  Fun.Project _ callee field -> renderExpr callee <> "." <> pretty field
  Fun.Primitive _ op args -> "#" <> pretty op <> renderArgs (map renderExpr args)
  Fun.Select _ scrutinee branches -> "case" <+> renderExpr scrutinee <+> "of" <+> blockLines (map renderBranch branches)
  Fun.Invoke _ name -> "invoke" <+> renderName name
  Fun.Fix _ name body -> hangEq ("fix" <+> renderName name) (renderExpr body)

renderBranch :: Fun.Branch -> Doc ann
renderBranch (Fun.Branch _ pat body) = group (renderPattern pat <+> "->" <> nest 2 (line <> renderExpr body))

-- * Core (Full\/Flat\/Join) IR

renderCoreFull :: Full.Program -> Text
renderCoreFull Full.Program {definitions} = renderDefs (map renderDef definitions)
  where
    renderDef (_, name, ret, stmt) =
      hangEq ("def" <+> renderName name <> renderArgs [renderName ret]) (renderFullStmt stmt)

renderFullStmt :: Full.Statement -> Doc ann
renderFullStmt = \case
  Full.Cut p c -> renderFullProd p <+> "~" <+> renderFullCons c
  Full.Primitive _ name ps c -> "#" <> pretty name <> renderArgs (map renderFullProd ps) <+> "~" <+> renderFullCons c
  Full.Invoke _ name c -> "invoke" <+> renderName name <+> "~" <+> renderFullCons c
  Full.ExternalCall _ name ps c -> "extern" <+> pretty name <> renderArgs (map renderFullProd ps) <+> "~" <+> renderFullCons c
  Full.BinOp _ op l r c -> parens (renderFullProd l <+> pretty op <+> renderFullProd r) <+> "~" <+> renderFullCons c
  Full.Ifz _ cond t e -> "if0" <+> renderFullProd cond <+> "then" <+> block (renderFullStmt t) <+> "else" <+> block (renderFullStmt e)

renderFullProd :: Full.Producer -> Doc ann
renderFullProd = \case
  Full.Var _ name -> renderName name
  Full.Literal _ lit -> renderLit lit
  Full.Construct _ tag ps cs -> renderTag tag <> renderArgs (map renderFullProd ps <> map renderFullCons cs)
  Full.Lambda _ params stmt -> group ("\\" <> hsep (map renderName params) <+> "." <+> block (renderFullStmt stmt))
  Full.Object _ fields ->
    braceList [pretty k <> renderArgs [renderName ret] <+> "=" <+> block (renderFullStmt stmt) | (k, (ret, stmt)) <- Map.toList fields]
  Full.Do _ name stmt -> "do" <+> renderName name <+> "." <+> block (renderFullStmt stmt)
  Full.Mu _ name stmt -> "mu" <+> renderName name <+> "." <+> block (renderFullStmt stmt)

renderFullCons :: Full.Consumer -> Doc ann
renderFullCons = \case
  Full.Label _ name -> renderName name
  Full.Apply _ ps cs -> renderArgs (map renderFullProd ps <> map renderFullCons cs)
  Full.Project _ field c -> "." <> pretty field <+> "->" <+> renderFullCons c
  Full.Then _ name stmt -> group ("then" <+> renderName name <+> "->" <+> block (renderFullStmt stmt))
  Full.Finish _ -> "finish"
  Full.Select _ branches -> "select" <+> blockLines (map renderFullBranch branches)

renderFullBranch :: Full.Branch -> Doc ann
renderFullBranch (Full.Branch _ pat stmt) = group (renderPattern pat <+> "->" <> nest 2 (line <> renderFullStmt stmt))

renderFlat :: Flat.Program -> Text
renderFlat Flat.Program {definitions} = renderDefs (map renderDef definitions)
  where
    renderDef (_, name, ret, stmt) =
      hangEq ("def" <+> renderName name <> renderArgs [renderName ret]) (renderFlatStmt stmt)

renderFlatStmt :: Flat.Statement -> Doc ann
renderFlatStmt = \case
  Flat.Cut p c -> renderFlatProd p <+> "~" <+> renderFlatCons c
  Flat.Join _ name c s -> group ("join" <+> renderName name <+> "=" <+> renderFlatCons c) <> line <> "in" <+> renderFlatStmt s
  Flat.Primitive _ name ps c -> "#" <> pretty name <> renderArgs (map renderFlatProd ps) <+> "~" <+> renderFlatCons c
  Flat.Invoke _ name c -> "invoke" <+> renderName name <+> "~" <+> renderFlatCons c
  Flat.ExternalCall _ name ps c -> "extern" <+> pretty name <> renderArgs (map renderFlatProd ps) <+> "~" <+> renderFlatCons c
  Flat.BinOp _ op l r c -> parens (renderFlatProd l <+> pretty op <+> renderFlatProd r) <+> "~" <+> renderFlatCons c
  Flat.Ifz _ cond t e -> "if0" <+> renderFlatProd cond <+> "then" <+> block (renderFlatStmt t) <+> "else" <+> block (renderFlatStmt e)

renderFlatProd :: Flat.Producer -> Doc ann
renderFlatProd = \case
  Flat.Var _ name -> renderName name
  Flat.Literal _ lit -> renderLit lit
  Flat.Construct _ tag ps cs -> renderTag tag <> renderArgs (map renderFlatProd ps <> map renderFlatCons cs)
  Flat.Lambda _ params stmt -> group ("\\" <> hsep (map renderName params) <+> "." <+> block (renderFlatStmt stmt))
  Flat.Object _ fields ->
    braceList [pretty k <> renderArgs [renderName ret] <+> "=" <+> block (renderFlatStmt stmt) | (k, (ret, stmt)) <- Map.toList fields]
  Flat.Mu _ name stmt -> "mu" <+> renderName name <+> "." <+> block (renderFlatStmt stmt)

renderFlatCons :: Flat.Consumer -> Doc ann
renderFlatCons = \case
  Flat.Label _ name -> renderName name
  Flat.Apply _ ps cs -> renderArgs (map renderFlatProd ps <> map renderFlatCons cs)
  Flat.Project _ field c -> "." <> pretty field <+> "->" <+> renderFlatCons c
  Flat.Then _ name stmt -> group ("then" <+> renderName name <+> "->" <+> block (renderFlatStmt stmt))
  Flat.Finish _ -> "finish"
  Flat.Select _ branches -> "select" <+> blockLines (map renderFlatBranch branches)

renderFlatBranch :: Flat.Branch -> Doc ann
renderFlatBranch (Flat.Branch _ pat stmt) = group (renderPattern pat <+> "->" <> nest 2 (line <> renderFlatStmt stmt))

renderJoin :: Join.Program -> Text
renderJoin Join.Program {definitions} = renderDefs (map renderDef definitions)
  where
    renderDef (_, name, ret, stmt) =
      hangEq ("def" <+> renderName name <> renderArgs [renderName ret]) (renderJoinStmt stmt)

renderJoinStmt :: Join.Statement -> Doc ann
renderJoinStmt = \case
  Join.Cut p ret -> renderJoinProd p <+> "~" <+> renderName ret
  Join.Join _ name c s -> group ("join" <+> renderName name <+> "=" <+> renderJoinCons c) <> line <> "in" <+> renderJoinStmt s
  Join.Primitive _ name ps ret -> "#" <> pretty name <> renderArgs (map renderJoinProd ps) <+> "~" <+> renderName ret
  Join.Invoke _ name ret -> "invoke" <+> renderName name <+> "~" <+> renderName ret
  Join.ExternalCall _ name ps ret -> "extern" <+> pretty name <> renderArgs (map renderJoinProd ps) <+> "~" <+> renderName ret
  Join.BinOp _ op l r ret -> parens (renderJoinProd l <+> pretty op <+> renderJoinProd r) <+> "~" <+> renderName ret
  Join.Ifz _ cond t e -> "if0" <+> renderJoinProd cond <+> "then" <+> block (renderJoinStmt t) <+> "else" <+> block (renderJoinStmt e)

renderJoinProd :: Join.Producer -> Doc ann
renderJoinProd = \case
  Join.Var _ name -> renderName name
  Join.Literal _ lit -> renderLit lit
  Join.Construct _ tag ps rets -> renderTag tag <> renderArgs (map renderJoinProd ps <> map renderName rets)
  Join.Lambda _ params stmt -> group ("\\" <> hsep (map renderName params) <+> "." <+> block (renderJoinStmt stmt))
  Join.Object _ fields ->
    braceList [pretty k <> renderArgs [renderName ret] <+> "=" <+> block (renderJoinStmt stmt) | (k, (ret, stmt)) <- Map.toList fields]
  Join.Mu _ name stmt -> "mu" <+> renderName name <+> "." <+> block (renderJoinStmt stmt)

renderJoinCons :: Join.Consumer -> Doc ann
renderJoinCons = \case
  Join.Label _ name -> renderName name
  Join.Apply _ ps rets -> renderArgs (map renderJoinProd ps <> map renderName rets)
  Join.Project _ field ret -> "." <> pretty field <+> "->" <+> renderName ret
  Join.Then _ name stmt -> group ("then" <+> renderName name <+> "->" <+> block (renderJoinStmt stmt))
  Join.Finish _ -> "finish"
  Join.Select _ branches -> "select" <+> blockLines (map renderJoinBranch branches)

renderJoinBranch :: Join.Branch -> Doc ann
renderJoinBranch (Join.Branch _ pat stmt) = group (renderPattern pat <+> "->" <> nest 2 (line <> renderJoinStmt stmt))

-- * Zig backend ANF IR

renderZigIr :: Ir.Program -> Text
renderZigIr Ir.Program {funcs, entry} =
  renderDefs (map renderFunc funcs <> maybe [] (\e -> ["entry" <+> "=" <+> renderName e]) entry)

renderFunc :: Ir.Func -> Doc ann
renderFunc Ir.Func {name, kind, selfVar, params, body} =
  hangEq (renderKind kind <+> "fn" <+> renderName name <> renderArgs (selfArg <> map renderName params)) (renderBlock body)
  where
    selfArg = case kind of
      Ir.TopLevelFn -> []
      _ -> [renderName selfVar]

renderKind :: Ir.FuncKind -> Doc ann
renderKind = \case
  Ir.TopLevelFn -> "toplevel"
  Ir.ClosureFn -> "closure"
  Ir.FieldFn -> "field"

renderBlock :: Ir.Block -> Doc ann
renderBlock (Ir.Block stmts term) = vsepHard (map renderStmt stmts <> [renderTerm term])

renderStmt :: Ir.Stmt -> Doc ann
renderStmt = \case
  Ir.Let x e -> "let" <+> renderName x <+> "=" <+> renderIrExpr e
  Ir.Dup x -> "dup" <+> renderName x
  Ir.Drop x -> "drop" <+> renderName x
  Ir.DropReuse tok x arity -> "dropReuse" <+> renderName tok <+> "=" <+> renderName x <+> "/" <> pretty arity

renderIrExpr :: Ir.Expr -> Doc ann
renderIrExpr = \case
  Ir.Lit lit -> renderLit lit
  Ir.MkStruct tag ops -> renderTag tag <> renderArgs (map renderName ops)
  Ir.MkClosure fn ops -> "closure" <+> renderName fn <> renderArgs (map renderName ops)
  Ir.MkRecord fields ops ->
    "record" <> renderArgs [pretty f <+> "=" <+> renderName fn | (f, fn) <- fields] <+> "captures" <> renderArgs (map renderName ops)
  Ir.Prim name ops -> "#" <> pretty name <> renderArgs (map renderName ops)
  Ir.ReadPath path -> renderPath path
  Ir.ReadCapture self i -> renderName self <> ".cap" <> list [pretty i]
  Ir.Force v field -> renderName v <> "!" <> pretty field
  Ir.PanicExpr msg -> "panic" <+> dquotes (pretty msg)
  Ir.MkStructReuse tok tag ops -> "reuse" <+> renderName tok <+> renderTag tag <> renderArgs (map renderName ops)

renderPath :: Ir.Path -> Doc ann
renderPath = \case
  Ir.PRoot n -> renderName n
  Ir.PField p i -> renderPath p <> "." <> pretty i

renderTerm :: Ir.Terminator -> Doc ann
renderTerm = \case
  Ir.TApplyCo k v -> "return" <+> renderName k <> renderArgs [renderName v]
  Ir.TCallClosure f args -> "return" <+> renderName f <> renderArgs (map renderName args)
  Ir.TStaticCall fn args -> "return" <+> renderName fn <> renderArgs (map renderName args)
  Ir.TProject v field k -> "return" <+> renderName v <> "." <> pretty field <> renderArgs [renderName k]
  Ir.TReturn v -> "return" <+> renderName v
  Ir.TIf guard t e -> group ("if" <+> renderGuard guard <+> "then" <+> block (renderBlock t) <+> "else" <+> block (renderBlock e))
  Ir.TPanic msg -> "panic" <+> dquotes (pretty msg)

renderGuard :: Ir.Guard -> Doc ann
renderGuard = \case
  Ir.GAnd tests -> sep (punctuate " &&" (map renderTest tests))
  Ir.GIsZero v -> renderName v <+> "== 0"

renderTest :: Ir.Test -> Doc ann
renderTest = \case
  Ir.TKindIs path k -> renderPath path <> ".kind ==" <+> pretty k
  Ir.TTagEq path tag -> renderPath path <> ".tag ==" <+> renderTag tag
  Ir.TLitEq path lit -> renderPath path <+> "==" <+> renderLit lit

-- * Surface syntax (Parse\/Rename)

renderParsedModule :: Syntax.Module (Malgo Parse) -> Text
renderParsedModule (Syntax.Module _ (Syntax.ParsedDefinitions decls)) =
  renderDefs (map renderDecl decls)

renderBindGroup :: Syntax.BindGroup (Malgo Rename) -> Text
renderBindGroup Syntax.BindGroup {..} =
  renderDefs
    $ concat
      [ [renderImport m list' | (_, m, list') <- _imports],
        [renderTypeSynonym t ps ty | (_, t, ps, ty) <- _typeSynonyms],
        [renderDataDef t ps cons | (_, t, ps, cons) <- _dataDefs],
        [renderForeign n t | (_, n, t) <- _foreigns],
        [renderScSig f t | (_, f, t) <- _scSigs],
        [renderScDef f e | group' <- _scDefs, (_, f, e) <- group']
      ]

renderDecl :: (RenderId (XId x)) => Syntax.Decl x -> Doc ann
renderDecl = \case
  Syntax.ScDef _ f e -> renderScDef f e
  Syntax.ScSig _ f t -> renderScSig f t
  Syntax.DataDef _ t ps cons -> renderDataDef t ps cons
  Syntax.TypeSynonym _ t ps ty -> renderTypeSynonym t ps ty
  Syntax.Infix _ assoc prec op -> "infix" <+> renderAssoc assoc <+> pretty prec <+> renderId op
  Syntax.Foreign _ n t -> renderForeign n t
  Syntax.Import _ m list' -> renderImport m list'

renderScDef :: (RenderId (XId x)) => XId x -> Syntax.Expr x -> Doc ann
renderScDef f e = hangEq ("def" <+> renderId f) (renderExprSyn e)

renderScSig :: (RenderId (XId x)) => XId x -> Syntax.Type x -> Doc ann
renderScSig f t = "sig" <+> renderId f <+> ":" <+> renderType t

renderDataDef :: (RenderId (XId x)) => XId x -> [(Range, XId x)] -> [(Range, XId x, [Syntax.Type x])] -> Doc ann
renderDataDef t ps cons =
  hangEq
    ("data" <+> renderId t <+> hsep (map (renderId . snd) ps))
    (sep (punctuate " |" (map renderConDef cons)))
  where
    renderConDef (_, con, ts) = renderId con <> renderArgs (map renderType ts)

renderTypeSynonym :: (RenderId (XId x)) => XId x -> [XId x] -> Syntax.Type x -> Doc ann
renderTypeSynonym t ps ty = hangEq ("type" <+> renderId t <+> hsep (map renderId ps)) (renderType ty)

renderForeign :: (RenderId (XId x)) => XId x -> Syntax.Type x -> Doc ann
renderForeign n t = "foreign" <+> renderId n <+> ":" <+> renderType t

renderImport :: ModuleName -> ImportList -> Doc ann
renderImport m list' = "import" <+> pretty m <+> renderImportList list'
  where
    renderImportList All = ""
    renderImportList (Selected xs) = renderArgs (map pretty xs)
    renderImportList (As m') = "as" <+> pretty m'

renderAssoc :: Assoc -> Doc ann
renderAssoc = \case
  LeftA -> "infixl"
  RightA -> "infixr"
  NeutralA -> "infix"

renderType :: (RenderId (XId x)) => Syntax.Type x -> Doc ann
renderType = \case
  Syntax.TyApp _ t ts -> renderType t <> renderArgs (map renderType ts)
  Syntax.TyVar _ v -> renderId v
  Syntax.TyCon _ c -> renderId c
  Syntax.TyArr _ t1 t2 -> renderType t1 <+> "->" <+> renderType t2
  Syntax.TyTuple _ ts -> renderArgs (map renderType ts)
  Syntax.TyRecord _ kvs rowTail ->
    braceList ([pretty k <+> ":" <+> renderType v | (k, v) <- kvs] <> maybe [] (\r -> ["|" <+> renderType r]) rowTail)
  Syntax.TyBlock _ t -> block (renderType t)
  Syntax.TyBottom _ -> "!"
  Syntax.TyTilde _ t -> "~" <> renderType t
  Syntax.TyVariant _ cases rowTail ->
    list (map renderCase cases <> maybe [] (\r -> ["|" <+> renderType r]) rowTail)
    where
      renderCase (k, ts) = pretty k <> renderArgs (map renderType ts)

renderExprSyn :: (RenderId (XId x)) => Syntax.Expr x -> Doc ann
renderExprSyn = \case
  Syntax.Var _ id -> renderId id
  Syntax.Unboxed _ l -> renderSynLit l
  Syntax.Boxed _ l -> renderSynLit l
  Syntax.Apply _ e1 e2 -> renderExprSyn e1 <> renderArgs [renderExprSyn e2]
  Syntax.OpApp _ op e1 e2 -> renderExprSyn e1 <+> renderId op <+> renderExprSyn e2
  Syntax.Project _ e k -> renderExprSyn e <> "." <> pretty k
  Syntax.Fn _ cs -> "fn" <+> blockLines (punctuate " |" (map renderClause (toList cs)))
  Syntax.Tuple _ es -> renderArgs (map renderExprSyn es)
  Syntax.Record _ kvs -> braceList [pretty k <+> "=" <+> renderExprSyn v | (k, v) <- kvs]
  Syntax.List _ es -> list (map renderExprSyn es)
  Syntax.Ann _ e t -> renderExprSyn e <+> ":" <+> renderType t
  Syntax.Seq _ ss -> blockLines (map renderStmtSyn (toList ss))
  Syntax.Parens _ e -> parens (renderExprSyn e)
  Syntax.Codata _ clauses -> "codata" <+> blockLines [group (renderCoPat cp <+> "->" <> nest 2 (line <> renderExprSyn e)) | (cp, e) <- clauses]
  Syntax.Label _ name body -> "label" <+> renderId name <+> "." <+> renderExprSyn body
  Syntax.Goto _ value label -> "goto" <+> renderExprSyn value <+> renderExprSyn label

renderSynLit :: Syntax.Literal x -> Doc ann
renderSynLit = \case
  Syntax.Int32 n -> pretty n
  Syntax.Int64 n -> pretty n <> "L"
  Syntax.Float f -> pretty f <> "f"
  Syntax.Double d -> pretty d
  Syntax.Char c -> squotes (pretty c)
  Syntax.String s -> dquotes (pretty s)

renderStmtSyn :: (RenderId (XId x)) => Syntax.Stmt x -> Doc ann
renderStmtSyn = \case
  Syntax.Let _ var body -> hangEq ("let" <+> renderId var) (renderExprSyn body)
  Syntax.LetP _ pat body -> hangEq ("let" <+> renderPat pat) (renderExprSyn body)
  Syntax.With _ Nothing body -> "with" <+> renderExprSyn body
  Syntax.With _ (Just var) body -> hangEq ("with" <+> renderId var) (renderExprSyn body)
  Syntax.NoBind _ body -> renderExprSyn body

renderClause :: (RenderId (XId x)) => Syntax.Clause x -> Doc ann
renderClause (Syntax.Clause _ pats body) = group (hsep (map renderPat (toList pats)) <+> "->" <> nest 2 (line <> renderExprSyn body))

renderPat :: (RenderId (XId x)) => Syntax.Pat x -> Doc ann
renderPat = \case
  Syntax.VarP _ id -> renderId id
  Syntax.ConP _ id ps -> renderId id <> renderArgs (map renderPat ps)
  Syntax.TupleP _ ps -> renderArgs (map renderPat ps)
  Syntax.RecordP _ kps -> braceList [pretty k <+> "=" <+> renderPat p | (k, p) <- kps]
  Syntax.ListP _ ps -> list (map renderPat ps)
  Syntax.UnboxedP _ l -> renderSynLit l
  Syntax.BoxedP _ l -> renderSynLit l

renderCoPat :: (RenderId (XId x)) => Syntax.CoPat x -> Doc ann
renderCoPat = \case
  Syntax.HoleP _ -> "#"
  Syntax.ApplyP _ cp p -> renderCoPat cp <> renderArgs [renderPat p]
  Syntax.ProjectP _ cp field -> renderCoPat cp <> "." <> pretty field
