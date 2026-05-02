module Malgo.Monad (Flag (..), runMalgoM, runMalgoMWith) where

import Effectful (Eff, IOE, runEff)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local
import Malgo.Features
import Malgo.Module
import Malgo.Prelude

runMalgoMWith ::
  Flag ->
  FeatureFlags ->
  Eff
    '[ Reader Flag,
       State Uniq,
       Features,
       State Pragma,
       Workspace,
       IOE
     ]
    b ->
  IO b
runMalgoMWith flag features e = runEff $ runWorkspaceOnPwd do
  runReader flag e
    & evalState (Uniq 0)
    & runFeatures features
    & evalState @Pragma mempty

runMalgoM ::
  Flag ->
  Eff
    '[ Reader Flag,
       State Uniq,
       Features,
       State Pragma,
       Workspace,
       IOE
     ]
    b ->
  IO b
runMalgoM flag = runMalgoMWith flag mempty
