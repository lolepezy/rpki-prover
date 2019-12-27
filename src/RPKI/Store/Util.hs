{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveAnyClass       #-}

module RPKI.Store.Util where

import RPKI.Store.Base.Map (SMap(..))
import RPKI.Store.Base.MultiMap (SMultiMap(..))

import Lmdb.Types
import Lmdb.Connection
import RPKI.Store.Base.LMDB

import RPKI.Store.Stores

createObjectStore :: Env -> IO (RpkiObjectStore LmdbStorage)
createObjectStore e = do
    let lmdb = LmdbStorage e
    objMap <- create e
    akiIndex <- createMulti e
    mftAkiIndex <- createMulti e
    
    return $ RpkiObjectStore {
      objects = SMap lmdb objMap,
      byAKI = SMultiMap lmdb akiIndex,
      mftByAKI = SMultiMap lmdb mftAkiIndex
    }   

mkLmdb :: FilePath -> IO Env
mkLmdb path = initializeReadWriteEnvironment mapSize readerNum maxDatabases path
  where 
    mapSize = 8*1024*1024*1024
    readerNum = 120
    maxDatabases = 120
    
closeLmdb :: Environment e -> IO ()
closeLmdb = closeEnvironment