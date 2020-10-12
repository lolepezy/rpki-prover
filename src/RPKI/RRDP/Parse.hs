{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE QuasiQuotes                #-}

module RPKI.RRDP.Parse where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans
import           Control.Monad.Trans.Except

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Base64           as B64
import qualified Data.ByteString.Base16 as Hex
import qualified Data.List                        as List
import qualified Data.Text                        as Text



import           Data.String.Interpolate.IsString

import           Data.STRef

import           Text.Read                        (readMaybe)

import qualified Xeno.SAX                         as Xeno

import           RPKI.Domain
import           RPKI.Errors
import           RPKI.RRDP.Types
import           RPKI.Util



-- | Parse RRDP notification file from a strict bytestring.
-- (https://tools.ietf.org/html/rfc8182#section-3.5.1)
-- 
parseNotification :: BS.ByteString -> Either RrdpError Notification
parseNotification bs = runST $ do
    deltas       <- newSTRef []
    version      <- newSTRef Nothing
    snapshotUri  <- newSTRef Nothing
    snapshotHash <- newSTRef Nothing
    serial       <- newSTRef Nothing
    sessionId    <- newSTRef Nothing

    let parse = parseXml bs
            (\case
                ("notification", attributes) -> do
                    forAttribute attributes "session_id" NoSessionId $
                        \v -> lift $ writeSTRef sessionId $ Just $ SessionId $ toShortBS v
                    forAttribute attributes "serial" NoSerial $
                        \v -> parseSerial v (lift . writeSTRef serial . Just)
                    forAttribute attributes "version" NoVersion $
                        \v -> case parseInteger v of
                            Nothing -> throwE NoVersion
                            Just v' -> lift $ writeSTRef version  (Just $ Version v')
                ("snapshot", attributes) -> do
                    forAttribute attributes "hash" NoSnapshotHash $
                        \v  -> case makeHash v of
                            Nothing   -> throwE $ BadHash $ convert v
                            Just hash -> lift $ writeSTRef snapshotHash $ Just hash
                    forAttribute attributes "uri" NoSnapshotURI $
                        \v  -> lift $ writeSTRef snapshotUri $ Just $ URI $ convert v
                ("delta", attributes) -> do
                    uri    <- forAttribute attributes "uri" NoDeltaURI (lift . pure)
                    h      <- forAttribute attributes "hash" NoDeltaHash (lift . pure)
                    s      <- forAttribute attributes "serial" NoDeltaSerial (lift . pure)
                    serial <- parseSerial s (lift . pure)
                    case makeHash h of
                        Nothing   -> throwE $ BadHash $ convert h
                        Just hash -> lift $ modifySTRef deltas $ \ds ->
                                DeltaInfo (URI $ convert uri) hash serial : ds
                (_, _) -> pure ()
            )
            (\_ -> pure ())

    let notification = Notification <$>
                        valuesOrError version NoVersion <*>
                        valuesOrError sessionId NoSessionId <*>
                        valuesOrError serial NoSerial <*>
                        (SnapshotInfo <$> 
                            valuesOrError snapshotUri NoSnapshotURI <*>
                            valuesOrError snapshotHash NoSnapshotHash) <*>
                        (reverse <$> (lift . readSTRef) deltas)

    runExceptT (parse >> notification)


-- | Parse RRDP snapshot from a strict bytestring.
-- (https://tools.ietf.org/html/rfc8182#section-3.5.2)
--
parseSnapshot :: BS.ByteString -> Either RrdpError Snapshot
parseSnapshot bs = runST $ do
    publishes <- newSTRef []
    sessionId <- newSTRef Nothing
    serial    <- newSTRef Nothing
    version   <- newSTRef Nothing

    let parse = parseXml bs
            (\case
                ("snapshot", attributes) -> do
                    forAttribute attributes "session_id" NoSessionId $
                        \v -> lift $ writeSTRef sessionId $ Just $ SessionId $ toShortBS v
                    forAttribute attributes "serial" NoSerial $
                        \v -> parseSerial v (lift . writeSTRef serial . Just)
                    forAttribute attributes "version" NoSerial $
                        \v -> case parseInteger v of
                            Nothing -> throwE NoVersion
                            Just v' -> lift $ writeSTRef version  (Just $ Version v')
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
                        valuesOrError version NoVersion <*>
                        valuesOrError sessionId NoSessionId <*>
                        valuesOrError serial NoSerial <*>
                        snapshotPublishes

    runExceptT (parse >> snapshot)


-- | Parse RRDP delta from a strict bytestring.
-- (https://tools.ietf.org/html/rfc8182#section-3.5.3)
--
parseDelta :: BS.ByteString -> Either RrdpError Delta
parseDelta bs = runST $ do
    deltaItems <- newSTRef []
    sessionId  <- newSTRef Nothing
    serial     <- newSTRef Nothing
    version    <- newSTRef Nothing
        
    let onElement = \case
            ("delta", attributes) -> do
                forAttribute attributes "session_id" NoSessionId $
                    \v -> lift $ writeSTRef sessionId $ Just $ SessionId $ toShortBS v
                forAttribute attributes "serial" NoSerial $
                    \v -> parseSerial v (lift . writeSTRef serial . Just)
                forAttribute attributes "version" NoSerial $
                    \v -> case parseInteger v of
                        Nothing -> throwE NoVersion
                        Just v' -> lift $ writeSTRef version  (Just $ Version v')

            ("publish", attributes) -> do
                uri <- forAttribute attributes "uri" NoPublishURI (lift . pure)
                hash <- case List.lookup "hash" attributes of
                    Nothing -> pure Nothing
                    Just h -> case makeHash h of
                        Nothing   -> throwE $ BadHash $ convert h
                        Just hash -> pure $ Just hash
                lift $ modifySTRef' deltaItems $ \ps -> Right (uri, hash, mempty) : ps

            ("withdraw", attributes) -> do
                uri <- forAttribute attributes "uri" NoPublishURI (lift . pure)
                h <- forAttribute attributes "hash" NoHashInWithdraw (lift . pure)
                case makeHash h of
                    Nothing   -> throwE $ BadHash $ convert h
                    Just hash -> lift $ modifySTRef' deltaItems $ \ps -> Left (uri, hash) : ps
                    
            (_, _) -> pure ()
            
    let onCharacterData base64 = 
            if BS.all isSpace_ base64 
                then pure ()
                else
                    (lift . readSTRef) deltaItems >>= \case
                        []        -> pure ()
                        item : is ->
                            case item of
                                Left  (uri, _) -> 
                                    throwE $ ContentInWithdraw $ Text.concat 
                                        [convert uri, ", c = ", convert base64]
                                Right (uri, hash, existing) -> do
                                    let base64' = BS.concat [existing, base64]
                                    lift $ writeSTRef deltaItems $ Right (uri, hash, base64') : is   

    let parse = parseXml bs onElement onCharacterData

    let deltaItems' = do
            is <- (lift . readSTRef) deltaItems
            pure $ map (\case
                    Left  (uri, hash) -> 
                        DW $ DeltaWithdraw (URI $ convert uri) hash                
                    Right (uri, hash, base64) -> 
                        DP $ DeltaPublish (URI $ convert uri) hash (EncodedBase64 $ removeSpaces base64))
                    $ reverse is


    let delta = Delta <$>
                    valuesOrError version NoVersion <*>
                    valuesOrError sessionId NoSessionId <*>
                    valuesOrError serial NoSerial <*>
                    deltaItems'

    runExceptT (parse >> delta)   


type Element = (BS.ByteString, [(BS.ByteString, BS.ByteString)])


-- | Generic convenience function to parse dimple version of XML used in RRDP by Xeno parser.
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
    

-- Utilities

forAttribute :: Monad m =>
                [(BS.ByteString, BS.ByteString)] ->
                BS.ByteString ->
                RrdpError ->
                (BS.ByteString -> ExceptT RrdpError m v) ->
                ExceptT RrdpError m v
forAttribute as name err f = case List.lookup name as of
    Nothing -> throwE err
    Just v  -> f v    


valuesOrError :: STRef s (Maybe v) -> RrdpError -> ExceptT RrdpError (ST s) v
valuesOrError v e =
    (lift . readSTRef) v >>= \case
        Nothing -> throwE e
        Just s  -> (lift . pure) s
     
parseInteger :: BS.ByteString -> Maybe Integer
parseInteger bs = readMaybe $ convert bs

parseSerial :: Monad m =>
            BS.ByteString ->
            (Serial -> ExceptT RrdpError m v) ->
            ExceptT RrdpError m v
parseSerial v f =  case parseInteger v of
    Nothing -> throwE $ BrokenSerial $ convert v
    Just s  -> f $ Serial s

makeHash :: BS.ByteString -> Maybe Hash
makeHash bs = mkHash . toBytes <$> hexString bs

-- Parsing HEX stuff
newtype HexString = HexString BS.ByteString 
    deriving (Show, Eq, Ord)

hexString :: BS.ByteString -> Maybe HexString
hexString bs = HexString <$> unhex bs

toBytes :: HexString -> BS.ByteString
toBytes (HexString bs) = bs

decodeBase64 :: Show c => EncodedBase64 -> c -> Either RrdpError DecodedBase64
decodeBase64 (EncodedBase64 bs) context = case B64.decodeBase64 bs of
    Left e  -> Left $ BadBase64 (e <> " for " <> Text.pack (show context)) $ convert bs
    Right b -> Right $! DecodedBase64 b
