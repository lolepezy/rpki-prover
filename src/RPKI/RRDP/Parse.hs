{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE BangPatterns     #-}

module RPKI.RRDP.Parse where

import           Control.Monad
import           Control.Monad.Trans.Except

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Short      as BSS
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Lazy       as LBS
import           Data.Hex                   (unhex)
import qualified Data.List                  as List
import qualified Data.Text                  as Text
import           Text.Read                  (readMaybe)

import qualified Text.XML.Expat.SAX         as X

import           RPKI.Domain
import           RPKI.Errors
import           RPKI.RRDP.Types

import           RPKI.Util                  (convert, trim, isSpace_, mkHash)


type NOTIF = (Maybe (Version, SessionId, Serial), Maybe SnapshotInfo, [DeltaInfo])

parseNotification :: LBS.ByteString -> Either RrdpError Notification
parseNotification xml = makeNotification =<< folded
    where
        makeNotification (Nothing, _, _) = Left NoSessionId
        makeNotification (_, Nothing, _) = Left NoSnapshotURI
        makeNotification (Just (v, sid, s), Just si, dis) = Right $ Notification v sid s si (reverse dis)

        folded = foldM foldElements (Nothing, Nothing, []) $ bs2Sax xml

        foldElements :: NOTIF -> SAX -> Either RrdpError NOTIF
        foldElements (Nothing, Nothing, []) (X.StartElement "notification" as) = (, Nothing, []) . Just <$> parseMeta as
        foldElements (_, _, _) (X.StartElement "notification" _) = Left $ BrokenXml "Misplaced 'notification' tag"

        foldElements (Just m, Nothing, _) (X.StartElement "snapshot" as) = (Just m, , []) . Just <$> parseSnapshotInfo as
        foldElements (Nothing, Nothing, _) (X.StartElement "snapshot" _) = Left $ BrokenXml "Misplaced 'snapshot' tag"

        foldElements (Just m, si, dis) (X.StartElement "delta" as) = do
            di <- parseDeltaInfo as
            let !z = di : dis
            pure (Just m, si, z)
        foldElements (Nothing, _, _) (X.StartElement "delta" _) = Left $ BrokenXml "Misplaced 'delta' tag"

        foldElements n _ = Right n


type SN = (Maybe (Version, SessionId, Serial), [SnapshotPublish])

parseSnapshot :: LBS.ByteString -> Either RrdpError Snapshot
parseSnapshot xml = makeSnapshot =<< folded
    where
        makeSnapshot (Nothing, _)           = Left NoSessionId
        makeSnapshot (Just (v, sid, s), ps) = Right $ Snapshot v sid s $ reverse ps

        folded = foldM foldPubs (Nothing, []) $ bs2Sax xml

        foldPubs :: SN -> SAX -> Either RrdpError SN
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


type DT = (Maybe (Version, SessionId, Serial), [DeltaItem])

parseDelta :: LBS.ByteString -> Either RrdpError Delta
parseDelta xml = makeDelta =<< folded
    where
        makeDelta (Nothing, _)           = Left NoSessionId
        makeDelta (Just (v, sid, s), ps) = Right $ Delta v sid s $ reverse ps

        folded = foldM foldItems (Nothing, []) $ bs2Sax xml

        foldItems :: DT -> SAX -> Either RrdpError DT
        foldItems (Nothing, ps) (X.StartElement "delta" as) = (,ps) . Just <$> parseMeta as
        foldItems (Just _, _)   (X.StartElement "delta" _)  = 
            Left $ BrokenXml "More than one 'delta'"
        foldItems (Nothing, _)  (X.StartElement "publish" _)  = 
            Left $ BrokenXml "'publish' before 'delta'"
        foldItems (Nothing, _)  (X.StartElement "withdraw" _)  = 
            Left $ BrokenXml "'withdraw' before 'delta'"            
        foldItems (Just sn, ps) (X.StartElement "publish" as) = do
            !dp <- parseDeltaPublish as
            pure (Just sn, let !z = DP dp : ps in z)
        foldItems (Just sn, ps) (X.StartElement "withdraw" as) = do
            !dw <- parseDeltaWithdraw as
            pure (Just sn, let !z = DW dw : ps in z)        
    
        foldItems (Just sn, []) (X.CharacterData _)            = Right (Just sn, [])
        foldItems (Just sn, DP (DeltaPublish uri' h (EncodedBase64 c)) : ps) (X.CharacterData cd) = 
            let !nc = c <> trim cd 
            in Right (Just sn, let !z = DP (DeltaPublish uri' h (EncodedBase64 nc)) : ps in z)
        foldItems d@(Just _, DW (DeltaWithdraw _ _) : _) (X.CharacterData cd) = 
            if BS.all isSpace_ cd 
                then let !z = Right d in z
                else Left $ ContentInWithdraw cd
        foldItems x  _                                          = Right x


parseInteger :: BS.ByteString -> Maybe Integer
parseInteger bs = readMaybe $ convert bs

parseSerial :: Monad m =>
            BS.ByteString ->
            (Serial -> ExceptT RrdpError m v) ->
            ExceptT RrdpError m v
parseSerial v f =  case parseInteger v of
    Nothing -> throwE $ BrokenSerial v
    Just s  -> f $ Serial s


parseMeta :: [(BS.ByteString, BS.ByteString)] -> Either RrdpError (Version, SessionId, Serial)
parseMeta as = do
    sId <- toEither NoSessionId $ List.lookup "session_id" as
    s   <- toEither NoSerial $ List.lookup "serial" as
    s'  <- toEither NoSerial $ parseInteger s
    v   <- toEither NoVersion $ List.lookup "version" as
    v'  <- toEither NoVersion $ parseInteger v            
    pure (Version v', SessionId $ BSS.toShort sId, Serial s')

parseSnapshotInfo :: [(BS.ByteString, BS.ByteString)] -> Either RrdpError SnapshotInfo
parseSnapshotInfo as = do
    u <- toEither NoSnapshotURI $ List.lookup "uri" as
    h <- toEither NoSnapshotHash $ List.lookup "hash" as        
    case makeHash h of
        Nothing -> Left $ BadHash h
        Just h' -> pure $ SnapshotInfo (URI $ convert u) h'

parseDeltaInfo :: [(BS.ByteString, BS.ByteString)] -> Either RrdpError DeltaInfo
parseDeltaInfo as = do
    u <- toEither NoSnapshotURI $ List.lookup "uri" as
    h <- toEither NoSnapshotHash $ List.lookup "hash" as      
    s <- toEither NoSerial $ List.lookup "serial" as
    s' <- toEither NoSerial $ parseInteger s  
    case makeHash h of
        Nothing -> Left $ BadHash h
        Just h' -> pure $ DeltaInfo (URI $ convert u) h' (Serial s')

parseDeltaPublish :: [(BS.ByteString, BS.ByteString)] -> Either RrdpError DeltaPublish
parseDeltaPublish as = do
    u <- toEither NoPublishURI $ List.lookup "uri" as
    case List.lookup "hash" as of 
        Nothing -> Right $ DeltaPublish (URI $ convert u) Nothing (EncodedBase64 "")
        Just h -> case makeHash h of
            Nothing -> Left $ BadHash h
            Just h' -> Right $ DeltaPublish (URI $ convert u) (Just h') (EncodedBase64 "")

parseDeltaWithdraw :: [(BS.ByteString, BS.ByteString)] -> Either RrdpError DeltaWithdraw
parseDeltaWithdraw as = do
    u <- toEither NoPublishURI $ List.lookup "uri" as
    h <- toEither NoHashInWithdraw $ List.lookup "hash" as  
    case makeHash h of
        Nothing -> Left $ BadHash h    
        Just h' -> pure $ DeltaWithdraw (URI $ convert u) h'

-- Some auxiliary things

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
    Left e -> Left $ BadBase64 (e <> " for " <> Text.pack (show context)) bs
    Right b -> Right $ DecodedBase64 b

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

type Element = (BS.ByteString, [(BS.ByteString, BS.ByteString)])
type SAX = X.SAXEvent BS.ByteString BS.ByteString


bs2Sax :: LBS.ByteString -> [SAX]
bs2Sax xml = filter nonEmpty_ saxs
    where 
        nonEmpty_ (X.CharacterData cd) = not $ BS.all isSpace_ cd
        nonEmpty_ _ = True        
        saxs = X.parse (X.defaultParseOptions :: X.ParseOptions BS.ByteString BS.ByteString) xml    