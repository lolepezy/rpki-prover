{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE BangPatterns     #-}

module RPKI.RRDP.Parse where

import           Control.Monad
import           Control.Monad.Trans.Except

import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Lazy       as BL
import           Data.Hex                   (unhex)
import qualified Data.List                  as L
import qualified Data.Text                  as T
import           Text.Read                  (readMaybe)

import qualified Text.XML.Expat.SAX         as X

import           RPKI.Domain
import           RPKI.RRDP.Types

import           RPKI.Util                  (convert, trim, isSpace_)


type NOTIF = (Maybe (Version, SessionId, Serial), Maybe SnapshotInfo, [DeltaInfo])

parseNotification :: BL.ByteString -> Either RrdpError Notification
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

parseSnapshot :: BL.ByteString -> Either RrdpError Snapshot
parseSnapshot xml = makeSnapshot =<< folded
    where
        makeSnapshot (Nothing, _) = Left NoSessionId
        makeSnapshot (Just (v, sid, s), ps) = Right $ Snapshot v sid s $ reverse ps

        folded = foldM foldPubs (Nothing, []) $ bs2Sax xml

        foldPubs :: SN -> SAX -> Either RrdpError SN
        foldPubs (Nothing, ps) (X.StartElement "snapshot" as) = (,ps) . Just <$> parseMeta as
        foldPubs (Just _, _) (X.StartElement "snapshot" _)  = 
            Left $ BrokenXml "More than one 'snapshot'"
        foldPubs (Nothing, _) (X.StartElement "publish" _)  = 
            Left $ BrokenXml "'publish' before 'snapshot'"
        foldPubs (Just sn, ps) (X.StartElement "publish" [("uri", uri)]) = 
            Right (Just sn, SnapshotPublish (URI $ convert uri) (EncodedBase64 "") : ps)
        foldPubs (Just _, []) (X.StartElement "publish" as) = 
            Left $ BrokenXml $ T.pack $ "Wrong atrribute set " <> show as
  
        foldPubs (Just sn, []) (X.CharacterData _)            = Right (Just sn, [])
        foldPubs (Just sn, SnapshotPublish uri (EncodedBase64 c) : ps) (X.CharacterData cd) = 
          let !nc = c <> trim cd
              !sp = SnapshotPublish uri (EncodedBase64 nc) : ps
            in Right (Just sn, sp)      
        foldPubs x  _                                          = Right x


type DT = (Maybe (Version, SessionId, Serial), [DeltaItem])

parseDelta :: BL.ByteString -> Either RrdpError Delta
parseDelta xml = makeDelta =<< folded
    where
        makeDelta (Nothing, _) = Left NoSessionId
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
            dp <- parseDeltaPublish as
            pure (Just sn, DP dp : ps)
        foldItems (Just sn, ps) (X.StartElement "withdraw" as) = do
            dw <- parseDeltaWithdraw as
            pure (Just sn, DW dw : ps)        
    
        foldItems (Just sn, []) (X.CharacterData _)            = Right (Just sn, [])
        foldItems (Just sn, DP (DeltaPublish uri h (EncodedBase64 c)) : ps) (X.CharacterData cd) = 
            let nc = c <> trim cd in Right (Just sn, DP (DeltaPublish uri h (EncodedBase64 nc)) : ps)
        foldItems d@(Just _, DW (DeltaWithdraw _ _) : _) (X.CharacterData cd) = 
            if B.all isSpace_ cd 
                then Right d 
                else Left $ ContentInWithdraw cd
        foldItems x  _                                          = Right x


parseInteger :: B.ByteString -> Maybe Integer
parseInteger bs = readMaybe $ convert bs

parseSerial :: Monad m =>
              B.ByteString ->
              (Serial -> ExceptT RrdpError m v) ->
              ExceptT RrdpError m v
parseSerial v f =  case parseInteger v of
    Nothing -> throwE $ BrokenSerial v
    Just s  -> f $ Serial s


parseMeta :: [(B.ByteString, B.ByteString)] -> Either RrdpError (Version, SessionId, Serial)
parseMeta as = do
    sId <- toEither NoSessionId $ L.lookup "session_id" as
    s   <- toEither NoSerial $ L.lookup "serial" as
    s'  <- toEither NoSerial $ parseInteger s
    v   <- toEither NoVersion $ L.lookup "version" as
    v'  <- toEither NoVersion $ parseInteger v            
    pure (Version v', SessionId $ sId, Serial s')

parseSnapshotInfo :: [(B.ByteString, B.ByteString)] -> Either RrdpError SnapshotInfo
parseSnapshotInfo as = do
    u <- toEither NoSnapshotURI $ L.lookup "uri" as
    h <- toEither NoSnapshotHash $ L.lookup "hash" as        
    case makeHash h of
        Nothing -> Left $ BadHash h
        Just h' -> pure $ SnapshotInfo (URI $ convert u) h'

parseDeltaInfo :: [(B.ByteString, B.ByteString)] -> Either RrdpError DeltaInfo
parseDeltaInfo as = do
    u <- toEither NoSnapshotURI $ L.lookup "uri" as
    h <- toEither NoSnapshotHash $ L.lookup "hash" as      
    s <- toEither NoSerial $ L.lookup "serial" as
    s' <- toEither NoSerial $ parseInteger s  
    case makeHash h of
        Nothing -> Left $ BadHash h
        Just h' -> pure $ DeltaInfo (URI $ convert u) h' (Serial s')

parseDeltaPublish :: [(B.ByteString, B.ByteString)] -> Either RrdpError DeltaPublish
parseDeltaPublish as = do
    u <- toEither NoPublishURI $ L.lookup "uri" as
    case L.lookup "hash" as of 
        Nothing -> Right $ DeltaPublish (URI $ convert u) Nothing (EncodedBase64 "")
        Just h -> case makeHash h of
            Nothing -> Left $ BadHash h
            Just h' -> Right $ DeltaPublish (URI $ convert u) (Just h') (EncodedBase64 "")

parseDeltaWithdraw :: [(B.ByteString, B.ByteString)] -> Either RrdpError DeltaWithdraw
parseDeltaWithdraw as = do
    u <- toEither NoPublishURI $ L.lookup "uri" as
    h <- toEither NoHashInWithdraw $ L.lookup "hash" as  
    case makeHash h of
        Nothing -> Left $ BadHash h    
        Just h' -> pure $ DeltaWithdraw (URI $ convert u) h'

-- Some auxiliary things

makeHash :: B.ByteString -> Maybe Hash
makeHash bs = Hash . toBytes <$> hexString bs

-- Parsing HEX stuff
newtype HexString = HexString B.ByteString 
    deriving (Show, Eq, Ord)

hexString :: B.ByteString -> Maybe HexString
hexString bs = HexString <$> unhex bs

toBytes :: HexString -> B.ByteString
toBytes (HexString bs) = bs

decodeBase64 :: Show c => EncodedBase64 -> c -> Either RrdpError DecodedBase64
decodeBase64 (EncodedBase64 bs) context = case B64.decode bs of
    Left e -> Left $ BadBase64 (e <> " for " <> show context) bs
    Right b -> Right $ DecodedBase64 b

toEither :: e -> Maybe v -> Either e v
toEither e Nothing  = Left e
toEither _ (Just v) = Right v

mapXmlElem :: (Element -> Either RrdpError a) ->
              (B.ByteString -> Either RrdpError a) ->
              X.SAXEvent B.ByteString B.ByteString -> 
              Maybe (Either RrdpError a)
mapXmlElem onElement onText = \case
    X.StartElement tag attrs -> Just $ onElement (tag, attrs)
    X.CharacterData text     -> Just $ onText text 
    X.FailDocument e         -> Just $ Left $ BrokenXml $ T.pack $ show e 
    _                        -> Nothing

type Element = (B.ByteString, [(B.ByteString, B.ByteString)])
type SAX = X.SAXEvent B.ByteString B.ByteString


bs2Sax :: BL.ByteString -> [SAX]
bs2Sax xml = filter nonEmpty_ saxs
    where 
        nonEmpty_ (X.CharacterData cd) = not $ B.all isSpace_ cd
        nonEmpty_ _ = True        
        saxs = X.parse (X.defaultParseOptions :: X.ParseOptions B.ByteString B.ByteString) xml    