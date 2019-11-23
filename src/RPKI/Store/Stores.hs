{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module RPKI.Store.Stores where

import RPKI.Domain
import RPKI.Store.Base.Map (SMap(..))
import qualified RPKI.Store.Base.Map as SM
import RPKI.Store.Base.Storage

newtype RpkiObjectStore s = RpkiObjectStore (SMap s Hash SValue)  

instance Storage s => WithStorage s (RpkiObjectStore s) where
  storage (RpkiObjectStore s) = storage s


getByHash :: Storage s => 
              Tx s m -> RpkiObjectStore s -> Hash -> IO (Maybe RpkiObject)
getByHash tx (RpkiObjectStore s) h = (fromSValue <$>) <$> SM.get tx s h

putObject :: Storage s => 
            Tx s 'RW -> RpkiObjectStore s -> Hash -> SValue -> IO ()
putObject tx (RpkiObjectStore s) h sv = SM.put tx s h sv

deleteObject :: Storage s => 
                Tx s 'RW -> RpkiObjectStore s -> Hash -> IO ()
deleteObject tx (RpkiObjectStore s) h = SM.delete tx s h


newtype TAStore s = TAStore (SMap s TaName TA)

instance Storage s => WithStorage s (TAStore s) where
  storage (TAStore s) = storage s


putTA :: Storage s => Tx s 'RW -> TAStore s -> TA -> IO ()
putTA tx (TAStore s) ta = SM.put tx s (taName ta) ta

getTA :: Storage s => Tx s m -> TAStore s -> TaName -> IO (Maybe TA)
getTA tx (TAStore s) name = SM.get tx s name
