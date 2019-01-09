{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RPKI.Store where

import qualified Data.Set as S
import qualified Data.Map as M

import Control.Concurrent.STM

import Data.Proxy

import Data.IxSet.Typed

import RPKI.Types


type EntryIxs = '[ AKI, SKI, Hash, URI ]
type IxEntry  = IxSet EntryIxs RpkiUnit

instance Indexable EntryIxs RpkiUnit where
    indices = ixList
        (ixGen (Proxy :: Proxy AKI))        
        (ixGen (Proxy :: Proxy SKI))
        (ixGen (Proxy :: Proxy Hash))
        (ixGen (Proxy :: Proxy URI))        


data Store = Store {
    entries :: TVar IxEntry
}