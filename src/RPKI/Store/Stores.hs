module RPKI.Store.Stores where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)

import RPKI.Domain


newtype Store k v = Store (TVar (Map k v))

withStore :: Store k v -> (Map k v -> Map k v) -> STM ()
withStore (Store t) = modifyTVar' t    

newtype TAStore = TAStore (Store String TA)