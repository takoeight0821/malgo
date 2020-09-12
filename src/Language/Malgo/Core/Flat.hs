{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Core.Flat
  ( Flat, flat
  )
where

import Language.Malgo.IR.Core
import Language.Malgo.Id
import Language.Malgo.Pass
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.CType

data Flat

instance Pass Flat (Exp (Id CType)) (Exp (Id CType)) where
  passName = "flat"
  trans e = pure $ flat e

flat :: Exp a -> Exp a
flat e = runIdentity $ runFlat (flatExp e)

runFlat :: Monad m => WriterT (Endo r) m r -> m r
runFlat = fmap (uncurry (flip appEndo)) . runWriterT

flatExp :: Monad m => Exp a -> WriterT (Endo (Exp a)) m (Exp a)
flatExp (Let ds e) = do
  ds <- traverse (rtraverse (appObj (runFlat . flatExp))) ds
  tell $ Endo $ \k -> Let ds k
  flatExp e
flatExp (Match v (Unpack con ps e :| [])) = do
  v <- flatExp v
  tell $ Endo $ \k -> Match v (Unpack con ps k :| [])
  flatExp e
flatExp (Match v (Bind x e :| [])) = do
  v <- flatExp v
  tell $ Endo $ \k -> Match v (Bind x k :| [])
  flatExp e
flatExp (Match e cs) =
  Match <$> flatExp e <*> traverse (appCase (runFlat . flatExp)) cs
flatExp e = pure e
