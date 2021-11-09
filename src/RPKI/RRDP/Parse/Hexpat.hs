{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module RPKI.RRDP.Parse.Hexpat where

import           Control.Monad

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.List                        as List
import qualified Data.Text                        as Text

import           Data.Tuple.Strict

import qualified Text.XML.Expat.SAX         as X

import           RPKI.RRDP.Parse.Common
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.RRDP.Types
import           RPKI.Util

import Debug.Trace


type NotificationFold = (Maybe (Version, SessionId, RrdpSerial), Maybe SnapshotInfo, [DeltaInfo])

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


type SnapshotFold = (Maybe (Version, SessionId, RrdpSerial), [SnapshotPublish])

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


type DeltaFold = T2 (Maybe (Version, SessionId, RrdpSerial)) [DeltaItem]

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

        foldItems d@(T2 (Just _) (DW (DeltaWithdraw uri _) : _)) (X.CharacterData cd) = 
            if BS.all isSpace_ cd 
                then let !z = Right d in z
                else Left $ ContentInWithdraw (convert uri) (convert cd)
                 
        foldItems x  _                                          = Right x



parseMeta :: [(BS.ByteString, BS.ByteString)] -> Either RrdpError (Version, SessionId, RrdpSerial)
parseMeta as = do
    sId <- toEither NoSessionId $ List.lookup "session_id" as
    s   <- toEither NoSerial $ List.lookup "serial" as
    s'  <- toEither NoSerial $ parseInteger s
    v   <- toEither NoVersion $ List.lookup "version" as
    v'  <- toEither NoVersion $ parseInteger v            
    pure (Version v', SessionId $ toShortBS sId, RrdpSerial s')

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
        Just h' -> pure $ DeltaInfo (URI $ convert u) h' (RrdpSerial s')

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
        saxs = X.parse optionsNoEntities xml
        optionsNoEntities = 
                X.defaultParseOptions { 
                    X.entityDecoder = Just (\e -> traceShow ("entity = " <> e) (Just e))
                } :: X.ParseOptions BS.ByteString BS.ByteString        
 

