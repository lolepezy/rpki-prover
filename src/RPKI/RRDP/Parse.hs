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
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Base64           as B64
import qualified Data.ByteString.Base16 as Hex
import qualified Data.List                        as List
import qualified Data.Text                        as Text



import           Data.String.Interpolate.IsString

import           Data.STRef

import           Text.Read                        (readMaybe)

import qualified Xeno.SAX                   as Xeno
import qualified Text.XML.Expat.SAX         as X

import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.RRDP.Types
import           RPKI.Util
import Data.Bifunctor
import RPKI.CommonTypes



-- | Parse RRDP notification file from a strict bytestring.
-- (https://tools.ietf.org/html/rfc8182#section-3.5.1)
-- 
parseNotification1 :: BS.ByteString -> Either RrdpError Notification
parseNotification1 bs = runST $ do
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
parseSnapshot1 :: BS.ByteString -> Either RrdpError Snapshot
parseSnapshot1 bs = runST $ do
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
parseDelta1 :: BS.ByteString -> Either RrdpError Delta
parseDelta1 bs = runST $ do
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
decodeBase64 (EncodedBase64 bs) context = 
    bimap 
        (\e -> BadBase64 (e <> " for " <> Text.pack (show context)) $ convert bs)
        DecodedBase64
        $ B64.decodeBase64 bs 


---------------------------------

type NotificationFold = (Maybe (Version, SessionId, Serial), Maybe SnapshotInfo, [DeltaInfo])

parseNotification :: LBS.ByteString -> Either RrdpError Notification
parseNotification xml = makeNotification =<< folded
    where
        makeNotification (Nothing, _, _) = Left NoSessionId
        makeNotification (_, Nothing, _) = Left NoSnapshotURI
        makeNotification (Just (v, sid, s), Just si, dis) = Right $ Notification v sid s si (reverse dis)

        folded = foldM foldElements (Nothing, Nothing, []) $ bs2Sax xml

        foldElements :: NotificationFold -> SAX -> Either RrdpError NotificationFold
        foldElements (Nothing, Nothing, []) (X.StartElement "notification" as) = 
            (, Nothing, []) . Just <$> parseMeta as

        foldElements (_, _, _) (X.StartElement "notification" _) = 
            Left $ BrokenXml "Misplaced 'notification' tag"

        foldElements (Just m, Nothing, _) (X.StartElement "snapshot" as) = 
            (Just m, , []) . Just <$> parseSnapshotInfo as

        foldElements (Nothing, Nothing, _) (X.StartElement "snapshot" _) = 
            Left $ BrokenXml "Misplaced 'snapshot' tag"

        foldElements (Just m, si, dis) (X.StartElement "delta" as) = do
            di <- parseDeltaInfo as
            let !z = di : dis
            pure (Just m, si, z)

        foldElements (Nothing, _, _) (X.StartElement "delta" _) = 
            Left $ BrokenXml "Misplaced 'delta' tag"

        foldElements n _ = Right n


type SnapshotFold = (Maybe (Version, SessionId, Serial), [SnapshotPublish])

parseSnapshot :: LBS.ByteString -> Either RrdpError Snapshot
parseSnapshot xml = makeSnapshot =<< folded
    where
        makeSnapshot (Nothing, _)           = Left NoSessionId
        makeSnapshot (Just (v, sid, s), ps) = Right $ Snapshot v sid s $ reverse ps

        folded = foldM foldPubs (Nothing, []) $ bs2Sax xml

        foldPubs :: SnapshotFold -> SAX -> Either RrdpError SnapshotFold
        foldPubs (Nothing, ps) (X.StartElement "snapshot" as) = (,ps) . Just <$> parseMeta as
        foldPubs (Just _, _) (X.StartElement "snapshot" _)  = 
            Left $ BrokenXml "More than one 'snapshot'"
        foldPubs (Nothing, _) (X.StartElement "publish" _)  = 
            Left $ BrokenXml "'publish' before 'snapshot'"
        -- foldPubs (Just sn, ps) (X.StartElement "publish" [("uri", uri')]) = 
        --     Right (Just sn, let !z = SnapshotPublish (URI $ convert uri') (EncodedBase64 "") : ps in z)
        foldPubs (Just !sn, ps) (X.StartElement "publish" attributes) = 
            case List.lookup "uri" attributes of
                Nothing   -> Left $ BrokenXml $ Text.pack $ "Attribute list doesn't contain 'uri': " <> show attributes
                Just !uri' -> Right (Just sn, let !z = SnapshotPublish (URI $ convert uri') (EncodedBase64 "") : ps in z)

        foldPubs (Just !sn, []) (X.CharacterData _)            = Right (Just sn, [])
        foldPubs (Just !sn, SnapshotPublish uri' (EncodedBase64 c) : ps) (X.CharacterData cd) = 
            let !nc = c <> trim cd
                !sp = SnapshotPublish uri' (EncodedBase64 nc) : ps
                in Right (Just sn, sp)      
        foldPubs x  _                                          = Right x


type DeltaFold = T2 (Maybe (Version, SessionId, Serial)) [DeltaItem]

parseDelta :: LBS.ByteString -> Either RrdpError Delta
parseDelta xml = makeDelta =<< folded
    where
        makeDelta (T2 Nothing _)             = Left NoSessionId
        makeDelta (T2 (Just (v, sid, s)) ps) = Right $ Delta v sid s $ reverse ps

        folded = foldM foldItems (T2 Nothing []) $ bs2Sax xml

        foldItems :: DeltaFold -> SAX -> Either RrdpError DeltaFold
        foldItems (T2 Nothing ps) (X.StartElement "delta" as) = 
            (\m -> T2 (Just m) ps) <$> parseMeta as

        foldItems (T2 (Just _) _)   (X.StartElement "delta" _)  = 
            Left $ BrokenXml "More than one 'delta'"

        foldItems (T2 Nothing _)  (X.StartElement "publish" _)  = 
            Left $ BrokenXml "'publish' before 'delta'"

        foldItems (T2 Nothing _)  (X.StartElement "withdraw" _)  = 
            Left $ BrokenXml "'withdraw' before 'delta'"            

        foldItems (T2 (Just sn) ps) (X.StartElement "publish" as) = do
            !dp <- parseDeltaPublish as
            pure $! T2 (Just sn) (let !z = DP dp : ps in z)

        foldItems (T2 (Just sn) ps) (X.StartElement "withdraw" as) = do
            !dw <- parseDeltaWithdraw as
        
            pure $! T2 (Just sn) (let !z = DW dw : ps in z)        
    
        foldItems (T2 (Just sn) []) (X.CharacterData _)            = 
            pure $! T2 (Just sn) []

        foldItems (T2 (Just sn) (DP (DeltaPublish uri' h (EncodedBase64 c)) : ps)) (X.CharacterData cd) = 
            let !nc = c <> trim cd 
            in pure $! T2 (Just sn) (DP (DeltaPublish uri' h (EncodedBase64 nc)) : ps)

        foldItems d@(T2 (Just _) (DW (DeltaWithdraw _ _) : _)) (X.CharacterData cd) = 
            if BS.all isSpace_ cd 
                then let !z = Right d in z
                else Left $ ContentInWithdraw $ convert cd
                 
        foldItems x  _                                          = Right x




parseMeta :: [(BS.ByteString, BS.ByteString)] -> Either RrdpError (Version, SessionId, Serial)
parseMeta as = do
    sId <- toEither NoSessionId $ List.lookup "session_id" as
    s   <- toEither NoSerial $ List.lookup "serial" as
    s'  <- toEither NoSerial $ parseInteger s
    v   <- toEither NoVersion $ List.lookup "version" as
    v'  <- toEither NoVersion $ parseInteger v            
    pure (Version v', SessionId $ toShortBS sId, Serial s')

parseSnapshotInfo :: [(BS.ByteString, BS.ByteString)] -> Either RrdpError SnapshotInfo
parseSnapshotInfo as = do
    u <- toEither NoSnapshotURI $ List.lookup "uri" as
    h <- toEither NoSnapshotHash $ List.lookup "hash" as        
    case makeHash h of
        Nothing -> Left $ BadHash $ convert h
        Just h' -> pure $ SnapshotInfo (URI $ convert u) h'

parseDeltaInfo :: [(BS.ByteString, BS.ByteString)] -> Either RrdpError DeltaInfo
parseDeltaInfo as = do
    u <- toEither NoSnapshotURI $ List.lookup "uri" as
    h <- toEither NoSnapshotHash $ List.lookup "hash" as      
    s <- toEither NoSerial $ List.lookup "serial" as
    s' <- toEither NoSerial $ parseInteger s  
    case makeHash h of
        Nothing -> Left $ BadHash $ convert h
        Just h' -> pure $ DeltaInfo (URI $ convert u) h' (Serial s')

parseDeltaPublish :: [(BS.ByteString, BS.ByteString)] -> Either RrdpError DeltaPublish
parseDeltaPublish as = do
    u <- toEither NoPublishURI $ List.lookup "uri" as
    case List.lookup "hash" as of 
        Nothing -> Right $ DeltaPublish (URI $ convert u) Nothing (EncodedBase64 "")
        Just h -> case makeHash h of
            Nothing -> Left $ BadHash $ convert h
            Just h' -> Right $ DeltaPublish (URI $ convert u) (Just h') (EncodedBase64 "")

parseDeltaWithdraw :: [(BS.ByteString, BS.ByteString)] -> Either RrdpError DeltaWithdraw
parseDeltaWithdraw as = do
    u <- toEither NoPublishURI $ List.lookup "uri" as
    h <- toEither NoHashInWithdraw $ List.lookup "hash" as  
    case makeHash h of
        Nothing -> Left $ BadHash $ convert h    
        Just h' -> pure $ DeltaWithdraw (URI $ convert u) h'


toEither :: e -> Maybe v -> Either e v
toEither e Nothing  = Left e
toEither _ (Just v) = Right v

mapXmlElem :: (Element -> Either RrdpError a) ->
              (BS.ByteString -> Either RrdpError a) ->
              X.SAXEvent BS.ByteString BS.ByteString -> 
              Maybe (Either RrdpError a)
mapXmlElem onElement onText = \case
    X.StartElement tag attrs -> Just $ onElement (tag, attrs)
    X.CharacterData text     -> Just $ onText text 
    X.FailDocument e         -> Just $ Left $ BrokenXml $ Text.pack $ show e 
    _                        -> Nothing

type Element1 = (BS.ByteString, [(BS.ByteString, BS.ByteString)])
type SAX = X.SAXEvent BS.ByteString BS.ByteString


bs2Sax :: LBS.ByteString -> [SAX]
bs2Sax xml = filter nonEmpty_ saxs
    where 
        nonEmpty_ (X.CharacterData cd) = not $ BS.all isSpace_ cd
        nonEmpty_ _ = True        
        saxs = X.parse (X.defaultParseOptions :: X.ParseOptions BS.ByteString BS.ByteString) xml    
 

