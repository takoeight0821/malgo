module Malgo.Sequent.Core.Fingerprint
  ( fingerprintFlat,
    fingerprintJoin,
  )
where

import Data.List (intercalate, sort)
import Data.Map.Strict qualified as Map
import Malgo.Sequent.Core.Flat qualified as Flat
import Malgo.Sequent.Core.Join qualified as Join
import Prelude

-- | Compute a shape fingerprint for a Flat IR program.
-- The fingerprint is a single-line summary of structural statistics,
-- stable across formatting changes but sensitive to semantic structure changes.
fingerprintFlat :: Flat.Program -> String
fingerprintFlat (Flat.Program defs _) =
  let stats = foldMap flatDefStats defs
   in renderStats stats

-- | Compute a shape fingerprint for a Join IR program.
fingerprintJoin :: Join.Program -> String
fingerprintJoin (Join.Program defs _) =
  let stats = foldMap joinDefStats defs
   in renderStats stats

data Stats = Stats
  { cuts :: !Int,
    joins :: !Int,
    primitives :: !Int,
    invokes :: !Int,
    externalCalls :: !Int,
    binOps :: !Int,
    ifzs :: !Int,
    vars :: !Int,
    literals :: !Int,
    constructs :: !Int,
    lambdas :: !Int,
    objects :: !Int,
    mus :: !Int,
    cocases :: !Int,
    labels :: !Int,
    applies :: !Int,
    projects :: !Int,
    thens :: !Int,
    finishes :: !Int,
    selects :: !Int,
    destructors :: !Int,
    definitions :: !Int
  }

instance Semigroup Stats where
  a <> b =
    Stats
      { cuts = a.cuts + b.cuts,
        joins = a.joins + b.joins,
        primitives = a.primitives + b.primitives,
        invokes = a.invokes + b.invokes,
        externalCalls = a.externalCalls + b.externalCalls,
        binOps = a.binOps + b.binOps,
        ifzs = a.ifzs + b.ifzs,
        vars = a.vars + b.vars,
        literals = a.literals + b.literals,
        constructs = a.constructs + b.constructs,
        lambdas = a.lambdas + b.lambdas,
        objects = a.objects + b.objects,
        mus = a.mus + b.mus,
        cocases = a.cocases + b.cocases,
        labels = a.labels + b.labels,
        applies = a.applies + b.applies,
        projects = a.projects + b.projects,
        thens = a.thens + b.thens,
        finishes = a.finishes + b.finishes,
        selects = a.selects + b.selects,
        destructors = a.destructors + b.destructors,
        definitions = a.definitions + b.definitions
      }

instance Monoid Stats where
  mempty =
    Stats
      { cuts = 0,
        joins = 0,
        primitives = 0,
        invokes = 0,
        externalCalls = 0,
        binOps = 0,
        ifzs = 0,
        vars = 0,
        literals = 0,
        constructs = 0,
        lambdas = 0,
        objects = 0,
        mus = 0,
        cocases = 0,
        labels = 0,
        applies = 0,
        projects = 0,
        thens = 0,
        finishes = 0,
        selects = 0,
        destructors = 0,
        definitions = 0
      }

renderStats :: Stats -> String
renderStats s =
  let pairs =
        filter (\(_, v) -> v > 0) $
          sort
            [ ("applies", s.applies),
              ("binOps", s.binOps),
              ("cocases", s.cocases),
              ("constructs", s.constructs),
              ("cuts", s.cuts),
              ("definitions", s.definitions),
              ("destructors", s.destructors),
              ("externalCalls", s.externalCalls),
              ("finishes", s.finishes),
              ("ifzs", s.ifzs),
              ("invokes", s.invokes),
              ("joins", s.joins),
              ("labels", s.labels),
              ("lambdas", s.lambdas),
              ("literals", s.literals),
              ("mus", s.mus),
              ("objects", s.objects),
              ("primitives", s.primitives),
              ("projects", s.projects),
              ("selects", s.selects),
              ("thens", s.thens),
              ("vars", s.vars)
            ]
   in intercalate " " $ map (\(k, v) -> k <> "=" <> show v) pairs

-- Flat IR statistics

flatDefStats :: (a, b, c, Flat.Statement) -> Stats
flatDefStats (_, _, _, stmt) = mempty {definitions = 1} <> flatStmtStats stmt

flatStmtStats :: Flat.Statement -> Stats
flatStmtStats (Flat.Cut p c) = mempty {cuts = 1} <> flatProdStats p <> flatConsStats c
flatStmtStats (Flat.Join _ _ c s) = mempty {joins = 1} <> flatConsStats c <> flatStmtStats s
flatStmtStats (Flat.Primitive _ _ ps c) = mempty {primitives = 1} <> foldMap flatProdStats ps <> flatConsStats c
flatStmtStats (Flat.Invoke _ _ c) = mempty {invokes = 1} <> flatConsStats c
flatStmtStats (Flat.ExternalCall _ _ ps c) = mempty {externalCalls = 1} <> foldMap flatProdStats ps <> flatConsStats c
flatStmtStats (Flat.BinOp _ _ l r c) = mempty {binOps = 1} <> flatProdStats l <> flatProdStats r <> flatConsStats c
flatStmtStats (Flat.Ifz _ p t e) = mempty {ifzs = 1} <> flatProdStats p <> flatStmtStats t <> flatStmtStats e

flatProdStats :: Flat.Producer -> Stats
flatProdStats (Flat.Var _ _) = mempty {vars = 1}
flatProdStats (Flat.Literal _ _) = mempty {literals = 1}
flatProdStats (Flat.Construct _ _ ps cs) = mempty {constructs = 1} <> foldMap flatProdStats ps <> foldMap flatConsStats cs
flatProdStats (Flat.Lambda _ _ s) = mempty {lambdas = 1} <> flatStmtStats s
flatProdStats (Flat.Object _ fs) = mempty {objects = 1} <> foldMap (flatStmtStats . snd) (Map.elems fs)
flatProdStats (Flat.Mu _ _ s) = mempty {mus = 1} <> flatStmtStats s
flatProdStats (Flat.Cocase _ bs) = mempty {cocases = 1} <> foldMap (\(_, _, s) -> flatStmtStats s) bs

flatConsStats :: Flat.Consumer -> Stats
flatConsStats (Flat.Label _ _) = mempty {labels = 1}
flatConsStats (Flat.Apply _ ps cs) = mempty {applies = 1} <> foldMap flatProdStats ps <> foldMap flatConsStats cs
flatConsStats (Flat.Project _ _ c) = mempty {projects = 1} <> flatConsStats c
flatConsStats (Flat.Then _ _ s) = mempty {thens = 1} <> flatStmtStats s
flatConsStats (Flat.Finish _) = mempty {finishes = 1}
flatConsStats (Flat.Select _ bs) = mempty {selects = 1} <> foldMap (\(Flat.Branch _ _ s) -> flatStmtStats s) bs
flatConsStats (Flat.Destructor _ _ ps c) = mempty {destructors = 1} <> foldMap flatProdStats ps <> flatConsStats c

-- Join IR statistics

joinDefStats :: (a, b, c, Join.Statement) -> Stats
joinDefStats (_, _, _, stmt) = mempty {definitions = 1} <> joinStmtStats stmt

joinStmtStats :: Join.Statement -> Stats
joinStmtStats (Join.Cut p _) = mempty {cuts = 1} <> joinProdStats p
joinStmtStats (Join.Join _ _ c s) = mempty {joins = 1} <> joinConsStats c <> joinStmtStats s
joinStmtStats (Join.Primitive _ _ ps _) = mempty {primitives = 1} <> foldMap joinProdStats ps
joinStmtStats (Join.Invoke _ _ _) = mempty {invokes = 1}
joinStmtStats (Join.ExternalCall _ _ ps _) = mempty {externalCalls = 1} <> foldMap joinProdStats ps
joinStmtStats (Join.BinOp _ _ l r _) = mempty {binOps = 1} <> joinProdStats l <> joinProdStats r
joinStmtStats (Join.Ifz _ p t e) = mempty {ifzs = 1} <> joinProdStats p <> joinStmtStats t <> joinStmtStats e

joinProdStats :: Join.Producer -> Stats
joinProdStats (Join.Var _ _) = mempty {vars = 1}
joinProdStats (Join.Literal _ _) = mempty {literals = 1}
joinProdStats (Join.Construct _ _ ps _) = mempty {constructs = 1} <> foldMap joinProdStats ps
joinProdStats (Join.Lambda _ _ s) = mempty {lambdas = 1} <> joinStmtStats s
joinProdStats (Join.Object _ fs) = mempty {objects = 1} <> foldMap (joinStmtStats . snd) (Map.elems fs)
joinProdStats (Join.Mu _ _ s) = mempty {mus = 1} <> joinStmtStats s
joinProdStats (Join.Cocase _ bs) = mempty {cocases = 1} <> foldMap (\(_, _, s) -> joinStmtStats s) bs

joinConsStats :: Join.Consumer -> Stats
joinConsStats (Join.Label _ _) = mempty {labels = 1}
joinConsStats (Join.Apply _ ps _) = mempty {applies = 1} <> foldMap joinProdStats ps
joinConsStats (Join.Project _ _ _) = mempty {projects = 1}
joinConsStats (Join.Then _ _ s) = mempty {thens = 1} <> joinStmtStats s
joinConsStats (Join.Finish _) = mempty {finishes = 1}
joinConsStats (Join.Select _ bs) = mempty {selects = 1} <> foldMap (\(Join.Branch _ _ s) -> joinStmtStats s) bs
joinConsStats (Join.Destructor _ _ ps _) = mempty {destructors = 1} <> foldMap joinProdStats ps
