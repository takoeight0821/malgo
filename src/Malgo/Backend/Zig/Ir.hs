-- | The Zig backend's internal IR, produced by
-- 'Malgo.Backend.Zig.ClosureConv.convertProgram' from (normalized) Join IR
-- and printed to Zig text by 'Malgo.Backend.Zig.Emit.emitProgram'.
--
-- The IR is first-order and in ANF: every value is produced by a named
-- 'Let' and every operand position is a variable. Closure conversion has
-- already happened — captures are explicit index reads ('ReadCapture')
-- against the function's own closure object (the @self@ parameter of the
-- self-passing calling convention @fn (self, args) rt.Value@), and every
-- nested Lambda\/escaping join\/Object field has been lifted into its own
-- 'Func'. This is exactly the shape the Perceus pass needs: reference
-- counting reduces to counting variable occurrences.
--
-- 'Dup'\/'Drop' are only ever inserted by the Perceus pass;
-- 'Malgo.Backend.Zig.ClosureConv.convertProgram' never emits them.
module Malgo.Backend.Zig.Ir
  ( Program (..),
    Func (..),
    FuncKind (..),
    Block (..),
    Stmt (..),
    Expr (..),
    Path (..),
    Terminator (..),
    Guard (..),
    Test (..),
    freeVarsBlock,
    freeVarsStmt,
    freeVarsTerminator,
    freeVarsExpr,
    freeVarsGuard,
    suffixFreeVars,
    pathRoot,
    termOperands,
  )
where

-- Note on 'DropReuse'\/'MkStructReuse' (inserted only by
-- 'Malgo.Backend.Zig.Reuse'): a reuse token is an ordinary 'Name', not its
-- own 'Expr', because 'rt.dropReuse' returns @?rt.Value@ while every other
-- 'Expr' yields a plain @rt.Value@ -- keeping the two-instruction form
-- (rather than folding the pairing into one node) is also what keeps a
-- future interprocedural extension (threading a token through a closure's
-- captures) possible without another IR change.

import Data.Set qualified as Set
import Malgo.Prelude
import Malgo.Sequent.Fun (Literal, Name, Tag)

data Program = Program
  { funcs :: [Func],
    -- | The bootstrap function's name, when the compiled module has a
    -- top-level @main@ (mirroring 'Malgo.Sequent.Eval.evalProgram', a
    -- module without one compiles to a no-op executable).
    entry :: Maybe Name
  }
  deriving stock (Eq, Show)

-- | How a function receives its @self@ argument. 'TopLevelFn's are called
-- directly with the immortal @rt.no_self@ sentinel and ignore it;
-- 'ClosureFn's\/'FieldFn's receive the closure\/record object itself and
-- read their captures out of it. The distinction matters to Perceus:
-- only 'ClosureFn'\/'FieldFn' own (and must consume) their @self@.
data FuncKind = TopLevelFn | ClosureFn | FieldFn
  deriving stock (Eq, Show)

data Func = Func
  { range :: Range,
    name :: Name,
    kind :: FuncKind,
    -- | The name bound to the @self@ parameter. Referenced only by
    -- 'ReadCapture' reads and (after Perceus) a 'Drop'; unused for
    -- 'TopLevelFn'. The printer renders it as the literal @self@.
    selfVar :: Name,
    -- | Bound from @args[i]@ in order. A top-level definition's list is
    -- @[retName]@ (the return continuation).
    params :: [Name],
    body :: Block
  }
  deriving stock (Eq, Show)

data Block = Block [Stmt] Terminator
  deriving stock (Eq, Show)

data Stmt
  = Let Name Expr
  | -- | Inserted only by Perceus.
    Dup Name
  | -- | Inserted only by Perceus.
    Drop Name
  | -- | @const tok = rt.dropReuse(x, arity);@ — inserted only by
    -- 'Malgo.Backend.Zig.Reuse', replacing a plain 'Drop' whose value is
    -- about to be rebuilt by a same-arity 'MkStructReuse' later in this
    -- same statement list. Consumes @x@'s reference; binds @tok@ (an
    -- @?rt.Value@, non-null when the runtime recycled @x@'s allocation).
    DropReuse Name Name Int
  deriving stock (Eq, Show)

-- | Every 'Expr' yields exactly one @rt.Value@.
data Expr
  = -- | @rt.mk*@ — a fresh, owned allocation.
    Lit Literal
  | -- | @rt.mkStruct@ — consumes one reference per field.
    MkStruct Tag [Name]
  | -- | @rt.mkClosure(&fn, captures)@ — consumes one reference per capture.
    MkClosure Name [Name]
  | -- | @rt.mkRecord(fields, sharedCaptures)@ — ONE captures array shared
    -- by every field function (mirroring the evaluator's @Record env
    -- fields@); consumes one reference per capture.
    MkRecord [(Text, Name)] [Name]
  | -- | @rt.\<name\>(&.{args})@ — borrows its arguments, returns owned.
    -- Covers Join IR's @Primitive@, @ExternalCall@ and @BinOp@ uniformly.
    Prim Text [Name]
  | -- | Borrowed alias: a chain of @.payload.strukt.fields[i]@ reads
    -- (pattern-match bindings). No reference is created or consumed.
    ReadPath Path
  | -- | Borrowed: @rt.capturesOf(self)[i]@. Only appears as the leading
    -- statements of a 'ClosureFn'\/'FieldFn' body.
    ReadCapture Name Int
  | -- | @rt.forceField(v, field)@ — consumes one reference of the record,
    -- returns the field's (owned) value. Record fields are call-by-name
    -- ('Malgo.Sequent.Eval' re-runs the field statement per force), so
    -- forcing twice legitimately consumes two references.
    Force Name Text
  | -- | @rt.panicUnimplemented(..)@ (the 'Cocase' stub). @noreturn@: the
    -- printer truncates the rest of the block after it, and RcCheck treats
    -- it as terminating the path.
    PanicExpr Text
  | -- | @rt.mkStructReuse(tok, tag, fields)@ — inserted only by
    -- 'Malgo.Backend.Zig.Reuse'. Consumes the reuse token @tok@ (bound by
    -- a matching 'DropReuse') exactly once, plus one reference per field
    -- as usual; recycles @tok@'s allocation in place when the runtime
    -- marked it reusable, else falls back to a fresh 'MkStruct'.
    MkStructReuse Name Tag [Name]
  deriving stock (Eq, Show)

data Path = PRoot Name | PField Path Int
  deriving stock (Eq, Show)

pathRoot :: Path -> Name
pathRoot (PRoot n) = n
pathRoot (PField p _) = pathRoot p

-- | Every terminator except 'TIf'\/'TPanic' consumes one reference of each
-- of its operands (the call moves them into the callee).
data Terminator
  = -- | @return rt.applyCovalue(k, v)@
    TApplyCo Name Name
  | -- | @return rt.callClosure(f, &.{args})@
    TCallClosure Name [Name]
  | -- | @return \<fn\>(rt.no_self, &.{args})@ (Join IR's @Invoke@)
    TStaticCall Name [Name]
  | -- | @return rt.projectField(v, field, k)@
    TProject Name Text Name
  | -- | @return rt.applyDestructor(v, name, &.{args})@
    TDestruct Name Text [Name]
  | -- | @return v@ (Join IR's @Finish@; the generated @main@ owns the result)
    TReturn Name
  | -- | @Ifz@ and every lowered @Select@ arm. The guard only borrows.
    TIf Guard Block Block
  | -- | No-match fall-through and similar. RC-exempt (@noreturn@).
    TPanic Text
  deriving stock (Eq, Show)

data Guard
  = -- | Conjunction of borrowing tests; empty means always-true.
    GAnd [Test]
  | -- | @rt.isZero(v)@ (borrows).
    GIsZero Name
  deriving stock (Eq, Show)

data Test
  = -- | @path.kind == .\<kindName\>@
    TKindIs Path Text
  | -- | @path.kind == .strukt and rt.tagEq(path.payload.strukt.tag, tag)@
    TTagEq Path Tag
  | -- | kind + payload equality against a literal
    TLitEq Path Literal
  deriving stock (Eq, Show)

freeVarsBlock :: Block -> Set Name
freeVarsBlock (Block stmts terminator) = go stmts
  where
    go [] = freeVarsTerminator terminator
    go (Let x e : rest) = freeVarsExpr e <> Set.delete x (go rest)
    go (Dup x : rest) = Set.insert x (go rest)
    go (Drop x : rest) = Set.insert x (go rest)
    go (DropReuse tok x _ : rest) = Set.insert x (Set.delete tok (go rest))

-- | @suffixFreeVars stmts term !! i == freeVarsBlock (Block (drop i stmts) term)@
-- for every @i@ in @[0 .. length stmts]@, computed in one bottom-up pass.
-- Callers that walk a statement list and, at each position, need the free
-- variables of everything from there on (e.g. an unused-binding check)
-- would otherwise call 'freeVarsBlock' afresh on each shrinking suffix,
-- making that walk quadratic in the block's length; indexing into this
-- list instead keeps it linear.
suffixFreeVars :: [Stmt] -> Terminator -> [Set Name]
suffixFreeVars stmts term = scanr step (freeVarsTerminator term) stmts
  where
    step (Let x e) live = freeVarsExpr e <> Set.delete x live
    step (Dup x) live = Set.insert x live
    step (Drop x) live = Set.insert x live
    step (DropReuse tok x _) live = Set.insert x (Set.delete tok live)

freeVarsStmt :: Stmt -> Set Name
freeVarsStmt (Let _ e) = freeVarsExpr e
freeVarsStmt (Dup x) = Set.singleton x
freeVarsStmt (Drop x) = Set.singleton x
freeVarsStmt (DropReuse _ x _) = Set.singleton x

freeVarsExpr :: Expr -> Set Name
freeVarsExpr (Lit _) = Set.empty
freeVarsExpr (MkStruct _ vs) = Set.fromList vs
freeVarsExpr (MkClosure _ vs) = Set.fromList vs
freeVarsExpr (MkRecord _ vs) = Set.fromList vs
freeVarsExpr (Prim _ vs) = Set.fromList vs
freeVarsExpr (MkStructReuse tok _ vs) = Set.fromList (tok : vs)
freeVarsExpr (ReadPath p) = Set.singleton (pathRoot p)
freeVarsExpr (ReadCapture self _) = Set.singleton self
freeVarsExpr (Force v _) = Set.singleton v
freeVarsExpr (PanicExpr _) = Set.empty

freeVarsTerminator :: Terminator -> Set Name
freeVarsTerminator (TApplyCo k v) = Set.fromList [k, v]
freeVarsTerminator (TCallClosure f args) = Set.fromList (f : args)
freeVarsTerminator (TStaticCall _ args) = Set.fromList args
freeVarsTerminator (TProject v _ k) = Set.fromList [v, k]
freeVarsTerminator (TDestruct v _ args) = Set.fromList (v : args)
freeVarsTerminator (TReturn v) = Set.singleton v
freeVarsTerminator (TIf guard t e) = freeVarsGuard guard <> freeVarsBlock t <> freeVarsBlock e
freeVarsTerminator (TPanic _) = Set.empty

freeVarsGuard :: Guard -> Set Name
freeVarsGuard (GAnd tests) = Set.fromList (map (pathRoot . testPath) tests)
freeVarsGuard (GIsZero v) = Set.singleton v

testPath :: Test -> Path
testPath (TKindIs p _) = p
testPath (TTagEq p _) = p
testPath (TLitEq p _) = p

-- | Operands a terminator consumes one reference of ('TIf'\/'TPanic'
-- consume nothing), with multiplicity.
termOperands :: Terminator -> [Name]
termOperands = \case
  TApplyCo k v -> [k, v]
  TCallClosure f args -> f : args
  TStaticCall _ args -> args
  TProject v _ k -> [v, k]
  TDestruct v _ args -> v : args
  TReturn v -> [v]
  TIf {} -> []
  TPanic _ -> []
