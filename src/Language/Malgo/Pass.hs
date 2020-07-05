{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Pass where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Language.Malgo.Monad
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Text.PrettyPrint.HughesPJClass (Style (..), renderStyle, style)

class Pass p s t | p -> s t where
  passName :: Text
  isDump :: Opt -> Bool
  trans :: s -> MalgoM t

dump :: (MonadMalgo m, Show a, MonadIO m, Pretty a) => a -> m ()
dump x = do
  opt <- getOpt
  if isDebugMode opt
    then printLog $ TL.toStrict (pShow x)
    else printLog $ T.pack (renderStyle (style {lineLength = maxBound}) (pPrint x))

transWithDump :: forall p s t. (Pass p s t, Show t, Pretty t) => s -> MalgoM t
transWithDump s = do
  opt <- asks maOption
  t <- trans @p s
  when (isDump @p opt) $ do
    printLog $ "dump " <> passName @p
    dump t
  pure t
