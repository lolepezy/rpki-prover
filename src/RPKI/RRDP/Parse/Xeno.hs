{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module RPKI.RRDP.Parse.Xeno where

import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans
import           Control.Monad.Trans.Except

import           Control.Exception.Lifted

import qualified Data.ByteString                  as BS
import qualified Data.List                        as List

import           Data.String.Interpolate.IsString

import           Data.STRef

import qualified Xeno.SAX                   as Xeno

import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.RRDP.Types
import           RPKI.Util

import           RPKI.RRDP.Parse.Common

import           System.IO.Unsafe (unsafePerformIO)

catchExceptions :: Either RrdpError a -> Either RrdpError a
catchExceptions parser = 
    unsafePerformIO $ 
        evaluate parser 
        `catches` [Handler $ pure . Left . BrokenXml . fmtEx]


-- | Parse RRDP notification file from a strict bytestring.
-- (https://tools.ietf.org/html/rfc8182#section-3.5.1)
-- 
parseNotification :: BS.ByteString -> Either RrdpError Notification
parseNotification bs = catchExceptions $ runST $ do
    deltasRef       <- newSTRef []
    versionRef      <- newSTRef Nothing
    snapshotUriRef  <- newSTRef Nothing
    snapshotHashRef <- newSTRef Nothing
    serialRef       <- newSTRef Nothing
    sessionIdRef    <- newSTRef Nothing

    let parse = parseXml bs
            (\case
                ("notification", attributes) -> do
                    forAttribute attributes "session_id" NoSessionId $
                        \v -> lift $ writeSTRef sessionIdRef $ Just $ SessionId $ convert v
                    forAttribute attributes "serial" NoSerial $
                        \v -> parseSerial v (lift . writeSTRef serialRef . Just)
                    forAttribute attributes "version" NoVersionInNotification $
                        \v -> case parseInteger v of
                            Nothing -> throwE NoVersionInNotification
                            Just v' -> lift $ writeSTRef versionRef $ Just $ Version v'
                ("snapshot", attributes) -> do
                    forAttribute attributes "hash" NoSnapshotHash $
                        \v  -> case makeHash v of
                            Nothing    -> throwE $ BadHash $ convert v
                            Just hash' -> lift $ writeSTRef snapshotHashRef $ Just hash'
                    forAttribute attributes "uri" NoSnapshotURI $
                        \v  -> lift $ writeSTRef snapshotUriRef $ Just $ URI $ convert v
                ("delta", attributes) -> do
                    uri    <- forAttribute attributes "uri" NoDeltaURI (lift . pure)
                    h      <- forAttribute attributes "hash" NoDeltaHash (lift . pure)
                    s      <- forAttribute attributes "serial" NoDeltaSerial (lift . pure)
                    serial' <- parseSerial s (lift . pure)
                    case makeHash h of
                        Nothing    -> throwE $ BadHash $ convert h
                        Just hash' -> lift $ modifySTRef deltasRef $ \ds ->
                                DeltaInfo (URI $ convert uri) hash' serial' : ds
                (_, _) -> pure ()
            )
            (\_ -> pure ())

    let notification = Notification <$>
                        valuesOrError versionRef NoVersionInNotification <*>
                        valuesOrError sessionIdRef NoSessionId <*>
                        valuesOrError serialRef NoSerial <*>
                        (SnapshotInfo <$> 
                            valuesOrError snapshotUriRef NoSnapshotURI <*>
                            valuesOrError snapshotHashRef NoSnapshotHash) <*>
                        (reverse <$> (lift . readSTRef) deltasRef)

    runExceptT (parse >> notification)


-- | Parse RRDP snapshot from a strict bytestring.
-- (https://tools.ietf.org/html/rfc8182#section-3.5.2)
--
parseSnapshot :: BS.ByteString -> Either RrdpError Snapshot
parseSnapshot bs = catchExceptions $ runST $ do
    publishes <- newSTRef []
    sessionIdRef <- newSTRef Nothing
    serialRef    <- newSTRef Nothing
    versionRef   <- newSTRef Nothing

    let parse = parseXml bs
            (\case
                ("snapshot", attributes) -> do
                    forAttribute attributes "session_id" NoSessionId $
                        \v -> lift $ writeSTRef sessionIdRef $ Just $ SessionId $ convert v
                    forAttribute attributes "serial" NoSerial $
                        \v -> parseSerial v (lift . writeSTRef serialRef . Just)
                    forAttribute attributes "version" NoVersionInSnapshot $
                        \v -> case parseInteger v of
                            Nothing -> throwE NoVersionInSnapshot
                            Just v' -> lift $ writeSTRef versionRef  (Just $ Version v')
                ("publish", attributes) -> do
                    uri <- forAttribute attributes "uri" NoPublishURI (lift . pure)
                    lift $ modifySTRef publishes $ \pubs -> (uri, mempty) : pubs
                (_, _) -> pure ()
            )
            (\base64 ->                                
                (lift . readSTRef) publishes >>= \case
                    []               -> pure ()
                    (uri, existing) : pubs -> do
                        let base64' = BS.concat [existing, base64]
                        lift $ writeSTRef publishes $ (uri, base64') : pubs
            )

    let snapshotPublishes = do
            ps <- (lift . readSTRef) publishes
            pure $ map (\(uri, base64) -> 
                        SnapshotPublish (URI $ convert uri) (EncodedBase64 $ removeSpaces base64))
                        $ reverse ps

    let snapshot = Snapshot <$>
                        valuesOrError versionRef NoVersionInSnapshot <*>
                        valuesOrError sessionIdRef NoSessionId <*>
                        valuesOrError serialRef NoSerial <*>
                        snapshotPublishes

    runExceptT (parse >> snapshot)


-- | Parse RRDP delta from a strict bytestring.
-- (https://tools.ietf.org/html/rfc8182#section-3.5.3)
--
parseDelta :: BS.ByteString -> Either RrdpError Delta
parseDelta bs = catchExceptions $ runST $ do
    deltaItemsRef <- newSTRef []
    sessionIdRef  <- newSTRef Nothing
    serialRef     <- newSTRef Nothing
    versionRef    <- newSTRef Nothing
        
    let onElement = \case
            ("delta", attributes) -> do
                forAttribute attributes "session_id" NoSessionId $
                    \v -> lift $ writeSTRef sessionIdRef $ Just $ SessionId $ convert v
                forAttribute attributes "serial" NoSerial $
                    \v -> parseSerial v (lift . writeSTRef serialRef . Just)
                forAttribute attributes "version" NoVersionInDelta $
                    \v -> case parseInteger v of
                        Nothing -> throwE NoVersionInDelta
                        Just v' -> lift $ writeSTRef versionRef  (Just $ Version v')

            ("publish", attributes) -> do
                uri   <- forAttribute attributes "uri" NoPublishURI (lift . pure)
                hash' <- case List.lookup "hash" attributes of
                            Nothing -> pure Nothing
                            Just h  -> case makeHash h of
                                Nothing     -> throwE $ BadHash $ convert h
                                Just hash'' -> pure $ Just hash''                    
                lift $ modifySTRef' deltaItemsRef $ \ps -> Right (uri, hash', mempty) : ps

            ("withdraw", attributes) -> do
                uri <- forAttribute attributes "uri" NoPublishURI (lift . pure)
                h <- forAttribute attributes "hash" NoHashInWithdraw (lift . pure)
                case makeHash h of
                    Nothing    -> throwE $ BadHash $ convert h
                    Just hash' -> lift $ modifySTRef' deltaItemsRef $ \ps -> Left (uri, hash') : ps
                    
            (_, _) -> pure ()
            
    let onCharacterData base64 = 
            if BS.all isSpace_ base64 
                then pure ()
                else
                    (lift . readSTRef) deltaItemsRef >>= \case
                        []        -> pure ()
                        item : is ->
                            case item of
                                Left  (uri, _) -> 
                                    throwE $ ContentInWithdraw (convert uri) (convert base64)
                                Right (uri, hash', existing) -> do
                                    let base64' = BS.concat [existing, base64]
                                    lift $ writeSTRef deltaItemsRef $ Right (uri, hash', base64') : is   

    let parse = parseXml bs onElement onCharacterData

    let deltaItemsRef' = do
            is <- (lift . readSTRef) deltaItemsRef
            pure $ map (\case
                    Left  (uri, hash') -> 
                        DW $ DeltaWithdraw (URI $ convert uri) hash'
                    Right (uri, hash', base64) -> 
                        DP $ DeltaPublish (URI $ convert uri) hash' (EncodedBase64 $ removeSpaces base64))
                    $ reverse is


    let delta = Delta <$>
                    valuesOrError versionRef NoVersionInDelta <*>
                    valuesOrError sessionIdRef NoSessionId <*>
                    valuesOrError serialRef NoSerial <*>
                    deltaItemsRef'

    runExceptT (parse >> delta)   



-- | Generic convenience function to parse XML used in RRDP by Xeno parser.
-- 
-- We only care about tags, attributes and character data without taking into account nested 
-- structure or anything of that sort.
--
parseXml :: (PrimMonad pm) =>
            BS.ByteString ->
            (Element -> ExceptT RrdpError pm ()) ->
            (BS.ByteString -> ExceptT RrdpError pm ()) ->
            ExceptT RrdpError pm ()
parseXml bs onElement onText = do
    element <- lift $ stToPrim $ newSTRef (mempty, [])
    Xeno.process (processor element) bs
  where
    processor element = Xeno.Process {
        openF = \elemName -> 
                    lift $ stToPrim $ modifySTRef element (\(_, as) -> (elemName, as)),    
        attrF = \name value -> 
                    lift $ stToPrim $ modifySTRef element (\(n, as) -> (n, (name, value) : as)),

        endOpenF = \elemName -> do
                    e@(existingElemName, _) <- lift $ stToPrim $ readSTRef element
                    if existingElemName /= elemName 
                        then throwE $ BrokenXml [i|Expected closing tag for #{existingElemName}, but got #{elemName}.|]
                        else lift $ stToPrim $ writeSTRef element (elemName, [])
                    onElement e,    

        textF = onText,
        closeF = nothing,
        cdataF = nothing
    }
    nothing _ = pure ()
