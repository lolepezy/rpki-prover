{-# LANGUAGE UndecidableInstances #-}

-- | Validator helpers that don't fit into 'RPKI.AppMonad'.
module RPKI.AppMonadUtil where

import           Effectful
import           Effectful.Exception         (bracket)

import           RPKI.AppMonad

bracketVT :: (ValidatorIO es, IOE :> es)
          => IO a
          -> (a -> Eff es r)
          -> (a -> Eff es b)
          -> Eff es b
bracketVT acquire release f = bracket (liftIO acquire) release f
