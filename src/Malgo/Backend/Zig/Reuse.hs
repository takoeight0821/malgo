-- | Koka-style reuse-token insertion: a pure 'Ir.Program' → 'Ir.Program'
-- rewrite that runs AFTER Perceus (it needs Perceus's 'Ir.Drop' placement
-- to know which values die where) and BEFORE 'Malgo.Backend.Zig.RcCheck'
-- (which verifies the resulting token linearity as an ordinary part of its
-- static safety net).
--
-- Within a single straight-line statement list (never crossing a
-- 'Ir.TIf' branch — branch bodies are separate 'Ir.Block's and are
-- recursed into independently), pairs the nearest preceding 'Ir.Drop' with
-- a later 'Ir.MkStruct' (LIFO — the same pairing order Koka's own reuse
-- analysis uses), rewriting both to 'Ir.DropReuse'\/'Ir.MkStructReuse'. At
-- runtime, @rt.dropReuse@ recycles the dropped Object in place when it was
-- uniquely referenced (see @runtime\/zig\/runtime.zig@'s Object-reuse
-- semantics: struct, closure, and scalar payloads are all eligible, not
-- just same-shape structs — the backing fields\/captures array is kept
-- only when its length already matches the target arity, otherwise
-- reallocated), falling back to an ordinary drop and a null token
-- otherwise; @rt.mkStructReuse@ then either overwrites that recycled
-- Object or allocates fresh. No static arity check is needed here: every
-- pairing is a candidate, verified dynamically per call, and a
-- rewritten-but-never-taken fallback path is exactly as correct (if less
-- cheap) as the original 'Ir.Drop'\/'Ir.MkStruct' pair.
module Malgo.Backend.Zig.Reuse
  ( reuseProgram,
    reuseFunc,
  )
where

import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Malgo.Backend.Zig.Ir
import Malgo.Id (newTemporalId)
import Malgo.Module (ModuleName)
import Malgo.Prelude

reuseProgram :: (State Uniq :> es, Reader ModuleName :> es) => Program -> Eff es Program
reuseProgram program = do
  funcs <- traverse reuseFunc program.funcs
  pure program {funcs}

reuseFunc :: (State Uniq :> es, Reader ModuleName :> es) => Func -> Eff es Func
reuseFunc fn = do
  body <- reuseBlock fn.body
  pure fn {body}

reuseBlock :: (State Uniq :> es, Reader ModuleName :> es) => Block -> Eff es Block
reuseBlock (Block stmts terminator) = do
  stmts' <- pairStmts stmts
  terminator' <- case terminator of
    TIf guard t e -> TIf guard <$> reuseBlock t <*> reuseBlock e
    term -> pure term
  pure (Block stmts' terminator')

-- | Pairs each 'Drop' with the nearest LATER 'MkStruct' in this same
-- statement list, LIFO (the most recently seen still-unpaired drop wins).
-- A 'PanicExpr' is a barrier: the printer truncates everything after it,
-- so any pending drop cannot reach a pairing partner past it and is
-- flushed unchanged. Unpaired drops (no later MkStruct at all) are left
-- as plain 'Drop's.
pairStmts :: (State Uniq :> es, Reader ModuleName :> es) => [Stmt] -> Eff es [Stmt]
pairStmts = go []
  where
    -- `pending`: drops seen so far, not yet paired, most-recent-first.
    -- Anything still pending when the list ends never found a partner and
    -- must be flushed back out as plain drops -- these are real,
    -- already-computed-by-Perceus obligations; losing one here would leak.
    go pending [] = pure (map Drop (reverse pending))
    go pending (Let x (PanicExpr what) : rest) = do
      rest' <- go [] rest
      pure (map Drop (reverse pending) <> (Let x (PanicExpr what) : rest'))
    go pending (Drop x : rest) = go (x : pending) rest
    go (dropped : restPending) (Let x (MkStruct tag ops) : rest) = do
      tok <- newTemporalId "reuse"
      rest' <- go restPending rest
      pure (DropReuse tok dropped (length ops) : Let x (MkStructReuse tok tag ops) : rest')
    go pending (stmt : rest) = (stmt :) <$> go pending rest
