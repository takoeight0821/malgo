module Malgo.Monad (Flag (..), runMalgoM) where

import Effectful (Eff, IOE, runEff)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local
import Malgo.Features
import Malgo.Module
import Malgo.Prelude

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
runMalgoM flag e = runEff $ runWorkspaceOnPwd do
  runReader flag e
    & evalState (Uniq 0)
    & runFeatures mempty
    & evalState @Pragma mempty
