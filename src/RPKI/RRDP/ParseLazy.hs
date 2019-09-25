{-# LANGUAGE OverloadedStrings     #-}

module RPKI.RRDP.ParseLazy where

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef

import           Control.Monad.Primitive    (PrimMonad (..))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (isSpace, chr)
import qualified Data.List                   as L
import           Data.Hex                   (unhex)
import           Data.Word
import           Text.Read                  (readMaybe)

import qualified Text.XML.Expat.SAX as X

import           RPKI.Domain
import           RPKI.RRDP.Types
import           RPKI.Util                  (convert)

parseNotification :: BL.ByteString -> Either RrdpError Notification
parseNotification bs = runST $ do
    deltaInfos   <- newSTRef []
    version      <- newSTRef Nothing
    snapshotUri  <- newSTRef Nothing
    snapshotHash <- newSTRef Nothing
    serial       <- newSTRef Nothing
    sessionId    <- newSTRef Nothing

    let parse = parseLazyXml bs
            (\case
                ("notification", attributes) -> do
                    forAttribute attributes "session_id" NoSessionId $
                        \v -> lift $ writeSTRef sessionId $ Just $ SessionId  v
                    forAttribute attributes "serial" NoSerial $
                        \v -> parseSerial v (lift . writeSTRef serial . Just)
                    forAttribute attributes "version" NoVersion $
                        \v -> case parseInteger v of
                            Nothing -> throwE NoVersion
                            Just v' -> lift $ writeSTRef version  (Just $ Version v')
                ("snapshot", attributes) -> do
                    forAttribute attributes "hash" NoSnapshotHash $
                        \v  -> case makeHash v of
                            Nothing   -> throwE $ BadHash  v
                            Just hash -> lift $ writeSTRef snapshotHash $ Just hash
                    forAttribute attributes "uri" NoSnapshotURI $
                        \v  -> lift $ writeSTRef snapshotUri $ Just $ URI $ convert v
                ("delta", attributes) -> do
                    s       <- forAttribute attributes "serial" NoDeltaSerial (lift . pure)
                    uri     <- forAttribute attributes "uri" NoDeltaURI (lift . pure)
                    h       <- forAttribute attributes "hash" NoDeltaHash (lift . pure)                    
                    serial' <- parseSerial s (lift . pure)
                    case makeHash h of
                        Nothing   -> throwE $ BadHash  h
                        Just hash' -> lift $ modifySTRef deltaInfos $ \ds ->
                                DeltaInfo (URI (convert uri)) hash' serial' : ds
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
                        (reverse <$> (lift . readSTRef) deltaInfos)

    runExceptT (parse >> notification)


parseSnapshot :: BL.ByteString -> Either RrdpError Snapshot
parseSnapshot bs = runST $ do
    publishes <- newSTRef []
    sessionId <- newSTRef Nothing
    serial    <- newSTRef Nothing
    version   <- newSTRef Nothing

    let parse = parseLazyXml bs
            (\case
                ("snapshot", attributes) -> do
                    forAttribute attributes "session_id" NoSessionId $
                        \v -> lift $ writeSTRef sessionId $ Just $ SessionId  v
                    forAttribute attributes "serial" NoSerial $
                        \v -> parseSerial v (lift . writeSTRef serial . Just)
                    forAttribute attributes "version" NoSerial $
                        \v -> case parseInteger v of
                            Nothing -> throwE NoVersion
                            Just v' -> lift $ writeSTRef version  (Just $ Version v')
                ("publish", attributes) -> do
                    uri <- forAttribute attributes "uri" NoPublishURI (lift . pure)
                    lift $ modifySTRef' publishes $ \pubs -> (uri, Nothing) : pubs
                (_, _) -> pure ()
            )
            (\base64 ->                                
                (lift . readSTRef) publishes >>= \case
                    []               -> pure ()
                    (uri, cs) : pubs -> do
                        let base64' = appendBase64 base64 cs
                        lift $ writeSTRef publishes $ (uri, base64') : pubs
            )

    let snapshotPublishes = do
            ps <- (lift . readSTRef) publishes
            forM (reverse ps) $ \case 
                (uri, Nothing) -> throwE $ BadPublish  uri
                (uri, Just base64) ->
                    case toContent base64 of 
                        Left e    -> throwE $ BadBase64 $ convert e
                        Right content -> pure $ SnapshotPublish (URI $ convert uri) content

    let snapshot = Snapshot <$>
                    (valuesOrError version NoVersion) <*>
                    (valuesOrError sessionId NoSessionId) <*>
                    (valuesOrError serial NoSerial) <*>
                    snapshotPublishes

    runExceptT (parse >> snapshot)


parseDelta :: BL.ByteString -> Either RrdpError Delta
parseDelta bs = runST $ do
    items <- newSTRef []
    sessionId <- newSTRef Nothing
    serial    <- newSTRef Nothing
    version   <- newSTRef Nothing

    let parse = parseLazyXml bs
            (\case
                ("delta", attributes) -> do
                    forAttribute attributes "session_id" NoSessionId $
                        \v -> lift $ writeSTRef sessionId $ Just $ SessionId v
                    forAttribute attributes "serial" NoSerial $
                        \v -> parseSerial v (lift . writeSTRef serial . Just)
                    forAttribute attributes "version" NoSerial $
                        \v -> case parseInteger v of
                            Nothing -> throwE NoVersion
                            Just v' -> lift $ writeSTRef version  (Just $ Version v')

                ("publish", attributes) -> do
                    uri <- forAttribute attributes "uri" NoPublishURI (lift . pure)
                    hash <- case L.lookup "hash" attributes of
                        Nothing -> lift $ pure Nothing
                        Just h -> case makeHash h of                    
                            Nothing   -> throwE $ BadHash h
                            Just hash -> lift $ pure $ Just hash
                    lift $ modifySTRef' items $ \ps -> Right (uri, hash, Nothing) : ps

                ("withdraw", attributes) -> do
                    uri <- forAttribute attributes "uri" NoPublishURI (lift . pure)
                    h <- forAttribute attributes "hash" NoHashInWithdraw (lift . pure)
                    case makeHash h of                    
                        Nothing   -> throwE $ BadHash h
                        Just hash -> lift $ modifySTRef' items $ \ps -> Left (uri, hash) : ps
                (_, _) -> pure ()
            )
            (\base64 ->         
                if B.all isSpace_ base64 then 
                        lift $ pure ()                       
                    else 
                        (lift . readSTRef) items >>= \case
                            []        -> pure ()
                            item : is ->
                                case item of
                                    Left  (uri, _) -> throwE $ ContentInWithdraw $ B.concat [uri, ", c = " :: B.ByteString, base64]
                                    Right (uri, hash, content) -> do                            
                                        let base64' = appendBase64 base64 content
                                        lift $ writeSTRef items $ Right (uri, hash, base64') : is                        
            )

    let deltaItems = do
            is <- (lift . readSTRef) items
            forM (reverse is) $ \case 
                Left  (uri, hash)              -> pure $ DW $ DeltaWithdraw (URI $ convert uri) hash
                Right (uri, hash, Nothing)     -> throwE $ BadPublish  uri
                Right (uri, hash, Just base64) ->                     
                    case toContent base64 of 
                        Left e        -> throwE $ BadBase64 $ convert e
                        Right content -> pure $ DP $ DeltaPublish (URI $ convert uri) hash content

    let delta = Delta <$>
                    valuesOrError version NoVersion <*>
                    valuesOrError sessionId NoSessionId <*>
                    valuesOrError serial NoSerial <*>
                    deltaItems

    runExceptT (parse >> delta)

appendBase64 :: B.ByteString -> Maybe B.ByteString -> Maybe B.ByteString
appendBase64 base64 content = 
    maybe (Just base64) (Just . newContent) content
    where
        trimmed = trim base64
        newContent existing = case B.null trimmed of 
                True  -> existing
                False -> B.concat [existing, trimmed]
        base64' = (Just . maybe base64 newContent) content

parseInteger :: B.ByteString -> Maybe Integer
parseInteger bs = readMaybe $ convert bs

forAttribute :: Monad m =>
                [(B.ByteString, B.ByteString)] ->
                B.ByteString ->
                RrdpError ->
                (B.ByteString -> ExceptT RrdpError m v) ->
                ExceptT RrdpError m v
forAttribute as name err f = case L.lookup name as of
    Nothing -> throwE err
    Just v  -> f v

parseSerial :: Monad m =>
              B.ByteString ->
              (Serial -> ExceptT RrdpError m v) ->
              ExceptT RrdpError m v
parseSerial v f =  case parseInteger v of
    Nothing -> throwE $ BrokenSerial v
    Just s  -> f $ Serial s

valuesOrError :: STRef s (Maybe v) -> RrdpError -> ExceptT RrdpError (ST s) v
valuesOrError v e =
    (lift . readSTRef) v >>= \case
        Nothing -> throwE e
        Just s  -> (lift . pure) s

type Element = (B.ByteString, [(B.ByteString, B.ByteString)])


parseLazyXml :: (MonadTrans t, PrimMonad pm, Monad (t pm)) =>
            BL.ByteString ->
            (Element -> t pm ()) ->
            (B.ByteString -> t pm ()) ->
            t pm ()
parseLazyXml bs onElement onText = 
    forM_ (X.parse options bs) $ \case 
        X.StartElement tag attrs -> onElement (tag, attrs)
        X.CharacterData text     -> onText text 
        _ -> pure ()
    where
        options = X.defaultParseOptions :: X.ParseOptions B.ByteString B.ByteString

makeHash :: B.ByteString -> Maybe Hash
makeHash bs = Hash . toBytes <$> hexString bs

-- Parsing HEX stuff
newtype HexString = HexString B.ByteString 
    deriving (Show, Eq, Ord)

hexString :: B.ByteString -> Maybe HexString
hexString bs = HexString <$> unhex bs

toBytes :: HexString -> B.ByteString
toBytes (HexString bs) = bs


toContent :: B.ByteString -> Either String Content
toContent bs = Content <$> (B64.decode $ trim bs)    

concatContent :: [Content] -> Content
concatContent cs = Content $ B.concat $ map (\(Content c) -> c) cs

trim :: B.ByteString -> B.ByteString
trim bs = B.takeWhile (not . isSpace_) $ B.dropWhile isSpace_ bs    

isSpace_ :: Word8 -> Bool
isSpace_ = isSpace . chr . fromEnum