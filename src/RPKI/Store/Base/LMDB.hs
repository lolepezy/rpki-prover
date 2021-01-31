{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}


module RPKI.Store.Base.LMDB where

import Control.Monad (forM, forever)
import Control.Concurrent.STM
import Control.Exception

import qualified Data.ByteString as BS
import Data.Coerce (coerce)

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map

import RPKI.Store.Base.Storable
import RPKI.Store.Base.Storage
import RPKI.Util

import Data.IORef

import GHC.TypeLits
import Data.Proxy as P

import Lmdb.Connection
import Lmdb.Codec (byteString)
import qualified Lmdb.Map as LMap
import qualified Lmdb.Multimap as LMMap
import qualified Lmdb.Types as Lmdb

import Pipes
import RPKI.Reporting
import RPKI.AppMonad
import RPKI.Parallel

type Env = Lmdb.Environment 'Lmdb.ReadWrite
type DBMap = Lmdb.Database BS.ByteString BS.ByteString

data NativeEnv = ROEnv Env | RWEnv Env | Disabled

data LmdbEnv = LmdbEnv {
    nativeEnv :: TVar NativeEnv,
    txSem     :: Semaphore
}

data LmdbStore (name :: Symbol) = LmdbStore {     
    db  :: DBMap,
    env :: LmdbEnv
}

data LmdbMultiStore (name :: Symbol) = LmdbMultiStore { 
    db :: Lmdb.MultiDatabase BS.ByteString BS.ByteString,
    env :: LmdbEnv
}

type family LmdbTxMode (m :: TxMode) :: Lmdb.Mode where
    LmdbTxMode 'RO = 'Lmdb.ReadOnly
    LmdbTxMode 'RW = 'Lmdb.ReadWrite

toROTx :: Lmdb.Transaction (m :: Lmdb.Mode) -> Lmdb.Transaction 'Lmdb.ReadOnly
toROTx = coerce

class WithLmdb lmdb where
    getEnv :: lmdb -> LmdbEnv

newtype LmdbStorage = LmdbStorage { unEnv :: LmdbEnv }

instance WithLmdb LmdbStorage where
    getEnv = unEnv

instance WithTx LmdbStorage where    
    data Tx LmdbStorage (m :: TxMode) = LmdbTx (Lmdb.Transaction (LmdbTxMode m))

    readOnlyTx lmdb f = let
        e@LmdbEnv {..} = getEnv lmdb
        in withSemaphore txSem $ do 
                nEnv <- atomically $ getNativeEnv e
                withROTransaction nEnv (f . LmdbTx)        

    readWriteTx lmdb f = withTransaction1 (getEnv lmdb) (f . LmdbTx)        

withTransaction1 LmdbEnv {..} f = do        
        nEnv <- atomically $ do 
            readTVar nativeEnv >>= \case         
                Disabled     -> retry           
                ROEnv _      -> retry
                RWEnv native -> pure native
        withTransaction nEnv f


-- | Basic storage implemented using LMDB
instance Storage LmdbStorage where    
    type SMapImpl LmdbStorage = LmdbStore
    type SMultiMapImpl LmdbStorage = LmdbMultiStore

    put (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) (SValue (Storable bs)) = 
        LMap.repsert' tx db ks bs

    delete (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) = 
        LMap.delete' tx db ks

    get (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) =
        (SValue . Storable <$>) <$> LMap.lookup' (toROTx tx) db ks 

    foldS (LmdbTx tx) LmdbStore {..} f a0 =
        foldGeneric tx db f a0 withCursor LMap.firstForward        

    putMu (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) (SValue (Storable vs)) = 
        withMultiCursor tx db $ \c -> LMMap.insert c ks vs

    deleteMu (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) (SValue (Storable vs)) =   
        LMMap.deleteKV tx db ks vs

    deleteAllMu (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) = 
        withMultiCursor tx db $ \c ->
            LMMap.lookupFirstValue c ks >>= \case
                Nothing -> pure ()
                Just _  -> LMMap.deleteValues c

    foldMuForKey (LmdbTx tx) LmdbMultiStore {..} key@(SKey (Storable ks)) f a0 =
        withMultiCursor tx db $ \c -> do
            z <- newIORef a0
            runEffect $ LMMap.lookupValues c ks >-> do
                forever $ do
                    v <- await
                    lift $ do 
                        a <- readIORef z
                        !a' <- f a key (SValue $ Storable v)
                        writeIORef z a'
            readIORef z

    foldMu (LmdbTx tx) LmdbMultiStore {..} f a0 =
        foldGeneric tx db f a0 withMultiCursor LMMap.firstForward

-- TODO Add some nice type signature here
foldGeneric tx db f a0 withCurs makeProducer =
    withCurs tx db $ \c -> do
        z <- newIORef a0
        void $ runEffect $ makeProducer c >-> do
            forever $ do
                Lmdb.KeyValue k v <- await
                lift $ do 
                    a <- readIORef z
                    !a' <- f a (SKey $ Storable k) (SValue $ Storable v)
                    writeIORef z a'
        readIORef z


createLmdbStore :: forall name . KnownSymbol name => 
                    LmdbEnv -> IO (LmdbStore name)
createLmdbStore env@LmdbEnv {..} = do
    let name' = symbolVal (P.Proxy @name)
    db <- withTransaction1 env $ \tx -> openDatabase tx (Just name') defaultDbSettings     
    pure $ LmdbStore db env    

createLmdbMultiStore :: forall name . KnownSymbol name =>  
                        LmdbEnv -> IO (LmdbMultiStore name)
createLmdbMultiStore env@LmdbEnv {..} = do
    let name' = symbolVal (P.Proxy @name)
    db <- withTransaction1 env $ \tx -> openMultiDatabase tx (Just name') defaultMultiDbSettngs
    pure $ LmdbMultiStore db env    


defaultDbSettings :: Lmdb.DatabaseSettings BS.ByteString BS.ByteString
defaultDbSettings = makeSettings 
    (Lmdb.SortNative Lmdb.NativeSortLexographic) 
    byteString byteString

defaultMultiDbSettngs :: Lmdb.MultiDatabaseSettings BS.ByteString BS.ByteString
defaultMultiDbSettngs = makeMultiSettings 
    (Lmdb.SortNative Lmdb.NativeSortLexographic) 
    (Lmdb.SortNative Lmdb.NativeSortLexographic) 
    byteString byteString

-- Auxialliry stuff for limiting the amount of parallel LMDB transactions    
data Semaphore = Semaphore Int (TVar Int)

createSemaphore :: Int -> IO Semaphore
createSemaphore n = Semaphore n <$> newTVarIO 0

withSemaphore :: Semaphore -> IO a -> IO a
withSemaphore (Semaphore maxCounter current) f = 
    bracket incr decrement (const f)
    where 
        incr = atomically $ do 
            c <- readTVar current
            if c >= maxCounter 
                then retry
                else writeTVar current (c + 1)
        decrement _ = atomically $ modifyTVar' current $ \c -> c - 1


getNativeEnv :: LmdbEnv -> STM Env 
getNativeEnv LmdbEnv {..} = do 
    readTVar nativeEnv >>= \case        
        Disabled     -> retry            
        ROEnv native -> pure native
        RWEnv native -> pure native

data MapInfo a
    = Single a
    | Multi a
    deriving (Show, Eq, Ord)


-- | Copy all databases from the from LMDB environment to the other
-- This is a low-level operation to be used for de-fragmentation.
-- `dstN` is supposed to be a completely empty environment.
--
-- Do it in the pipline fashion: one thread for reading the old Env
-- and one thread for writing (k,v) pairs into the new Env.
-- 
copyEnv :: Env -> Env -> IO [(BS.ByteString, Int)]
copyEnv srcN dstN = do 

    mapNames <- withROTransaction srcN $ \srcTx -> do               
                    srcDb <- openDatabase srcTx Nothing defaultDbSettings    
                    getMapNames srcTx srcDb

    (bytes, _) <- mapException (AppException . storageError)        
                    $ bracketChanClosable 
                            1000
                            (liftIO . readKVs mapNames)
                            (liftIO . writeKVs)
                            (const $ pure ()) 
    pure bytes
    where
        readKVs mapNames queue = 
            withROTransaction srcN $ \srcTx ->
                forM mapNames $ \mapName -> do 
                    srcMap  <- openDatabase srcTx (Just $ convert mapName) defaultDbSettings           
                    isMulti <- isMultiDatabase srcTx srcMap                                   
                    if isMulti
                        then do 
                            closeDatabase srcN srcMap
                            srcMap' <- openMultiDatabase srcTx (Just $ convert mapName) defaultMultiDbSettngs
                            bytes <- writeMultiMapToQueue mapName srcMap' srcTx queue                            
                            pure (mapName, bytes)
                        else do 
                            bytes <- writeMapToQueue mapName srcMap srcTx queue
                            pure (mapName, bytes)
                            

        writeKVs queue = 
            withTransaction dstN $ \dstTx -> 
                go dstTx Map.empty 
          where
            go dstTx maps = do                              
                atomically (readCQueue queue) >>= \case 
                    Nothing -> pure ()
                    Just (mapInfo, key, value) -> do 
                        case maps !? mapInfo of 
                            Nothing -> 
                                case mapInfo of 
                                    Single mapName -> do 
                                        dstMap <- openDatabase dstTx (Just $ convert mapName) defaultDbSettings
                                        LMap.insertSuccess' dstTx dstMap key value
                                        go dstTx $ Map.insert mapInfo (Left dstMap) maps
                                    Multi mapName -> do 
                                        dstMap <- openMultiDatabase dstTx (Just $ convert mapName) defaultMultiDbSettngs
                                        go dstTx $ Map.insert mapInfo (Right dstMap) maps
                            Just (Left dstMap) -> do                                                                             
                                LMap.insertSuccess' dstTx dstMap key value
                                go dstTx maps
                            Just (Right multiDstMap) -> do                                             
                                withMultiCursor dstTx multiDstMap $ \dstC -> 
                                    LMMap.insert dstC key value
                                go dstTx maps      
    
        getMapNames tx db =
            withCursor tx db $ \c -> do 
                maps <- newIORef []
                void $ runEffect $ LMap.firstForward c >-> do
                    forever $ do
                        Lmdb.KeyValue name _ <- await
                        lift $ modifyIORef' maps ([name] <>)
                readIORef maps          
        
        writeMapToQueue mapName srcMap srcTx queue = do 
            bytes <- newIORef 0
            withCursor srcTx srcMap $ \c ->                
                void $ runEffect $ LMap.firstForward c >-> do
                    forever $ do
                        Lmdb.KeyValue key value <- await
                        lift $ atomically $ writeCQueue queue (Single mapName, key, value)
                        lift $ modifyIORef' bytes (+ (BS.length key + BS.length value))
            readIORef bytes

        writeMultiMapToQueue mapName srcMap srcTx queue = do
            bytes <- newIORef 0            
            withMultiCursor srcTx srcMap $ \srcC ->                 
                void $ runEffect $ LMMap.firstForward srcC >-> do
                    forever $ do
                        Lmdb.KeyValue key value <- await
                        lift $ atomically $ writeCQueue queue (Multi mapName, key, value)
                        lift $ modifyIORef' bytes (+ (BS.length key + BS.length value))
            readIORef bytes
