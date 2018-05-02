{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
module Language.Malgo.Monad where

import           Control.Lens           ((<+=))
import Data.Generics.Product
import           Data.IORef
import           Language.Malgo.Prelude
import           Text.PrettyPrint

-- TODO: replace with `ReaderT (IORef s) IO a`
newtype Malgo s a = Malgo { unMalgo :: StateT s IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState s
           , MonadIO
           )

class HasUniqSupply s where
  uniqSupply :: Lens s s Int Int
  default uniqSupply :: (Generic s, HasType Int s) => Lens s s Int Int
  uniqSupply = typed @Int

instance HasUniqSupply Int where
  uniqSupply = identity

newUniq :: HasUniqSupply s => Malgo s Int
newUniq = uniqSupply <+= 1

setUniq :: HasUniqSupply s => Int -> Malgo s ()
setUniq i = modify (set uniqSupply i)

getUniq :: HasUniqSupply s => Malgo s Int
getUniq = gets (view uniqSupply)

runMalgo :: (Default s, HasUniqSupply s) => Malgo s a -> IO (a, s)
runMalgo (Malgo m) = runStateT m def

malgoError :: Doc -> Malgo s a2
malgoError mes = liftIO $ die $ show mes

newMutVar :: a -> Malgo s (IORef a)
newMutVar x = Malgo $ lift $ newIORef x

readMutVar :: IORef a -> Malgo s a
readMutVar r = Malgo $ lift $ readIORef r

writeMutVar :: IORef a -> a -> Malgo s ()
writeMutVar r x = Malgo $ lift $ writeIORef r x