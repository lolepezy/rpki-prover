{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module RPKI.RRDP.Parse where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.ST
import           Data.STRef

import           Control.Lens               ((^.))

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy       as BL
import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Network.Wreq               as WR

import           Data.Word

import           Data.IORef
import           Text.Read                  (readMaybe)

import           Xeno.SAX                   as X

import           RPKI.Domain
import           RPKI.RRDP.Types
import           RPKI.Util                  (convert)



data NTags = NotificationTag | SnapshotTag | DeltaTag (Maybe DeltaInfo)

-- Download the notification
getNotifications :: URI -> IO (Either RrdpError Notification)
getNotifications (URI uri) = do
    r <- WR.get $ T.unpack uri
    pure $ parseNotification $ r ^. WR.responseBody


parseNotification :: BL.ByteString -> Either RrdpError Notification
parseNotification bs = runST $ do
    deltas       <- newSTRef []
    snapshotUri  <- newSTRef $ URI ""
    snapshotHash <- newSTRef $ Hash SHA256 ""
    serial       <- newSTRef $ Serial 0
    sessionId    <- newSTRef $ SessionId ""

    let parsed = parseXml (BL.toStrict bs)
            (\case
                ("notification", attributes) -> do
                    forAttribute attributes "session_id" NoSessionId $
                        \v -> lift $ writeSTRef sessionId $ SessionId v
                    forAttribute attributes "serial" NoSerial $
                        \v -> parseSerial v (lift . writeSTRef serial)
                ("snapshot", attributes) -> do
                    forAttribute attributes "hash" NoSnapshotHash $
                        \v  -> case makeHash v of
                            Nothing   -> throwE $ BadHash v
                            Just hash -> lift $ writeSTRef snapshotHash hash
                    forAttribute attributes "haurish" NoSnapshotURI $
                        \v  -> lift $ writeSTRef snapshotUri $ URI $ convert v
                ("delta", attributes) -> do
                    uri    <- forAttribute attributes "uri" NoDeltaURI (lift . pure)
                    h      <- forAttribute attributes "hash" NoDeltaHash (lift . pure)
                    s      <- forAttribute attributes "serial" NoDeltaSerial (lift . pure)
                    serial <- parseSerial s (lift . pure)
                    case makeHash h of
                        Nothing   -> throwE $ BadHash h
                        Just hash -> lift $ modifySTRef deltas $ \ds ->
                                            DeltaInfo (URI (convert uri)) hash serial : ds
            )
            (\_ -> pure ())

    notification <- Notification <$>
                        readSTRef sessionId <*>
                        readSTRef serial <*>
                        readSTRef snapshotUri <*>
                        readSTRef snapshotHash <*>
                        readSTRef deltas

    runExceptT (parsed >> pure notification)
    where
        forAttribute as name err f = case M.lookup name as of
            Nothing -> throwE err
            Just v  -> f v

        parseSerial v f =  case parseInteger v of
            Nothing -> throwE $ BrokenSerial v
            Just s  -> f $ Serial s

        makeHash bs = (Hash SHA256 . toBytes) <$> hexString bs


-- parseSnapshot :: BL.ByteString -> Either RrdpError Snapshot
-- parseSnapshot bs = pure Snapshot{}


parseInteger :: B.ByteString -> Maybe Integer
parseInteger bs = readMaybe $ convert bs

type Element = (B.ByteString, M.Map B.ByteString B.ByteString)

parseXml :: (MonadTrans t, Monad (t (ST s))) => 
            B.ByteString ->
            (Element -> t (ST s) ()) ->
            (B.ByteString -> t (ST s) ()) ->
            t (ST s) ()
parseXml bs onElement onText = do
    element <- lift $ newSTRef ("" :: B.ByteString, M.empty)
    X.process
        (\elemName -> lift $ modifySTRef' element (\(_, as) -> (elemName, as)))
        (\name value -> lift $ modifySTRef' element (\(n, as) -> (n, M.insert name value as)))
        (\elemName -> (lift $ readSTRef element) >>= onElement)
        onText
        nothing
        nothing
        bs
    where nothing = (\_ -> lift $ pure ())


-- Parsing HEX stuff
newtype HexString = HexString B.ByteString deriving ( Show, Eq, Ord )

hexString :: B.ByteString -> Maybe HexString
hexString bs = case B.all isValidHex bs of
    True  -> Just $ HexString bs
    False -> Nothing
    where  isValidHex c
            | (48 <= c) && (c < 58)  = True
            | (97 <= c) && (c < 103) = True
            | otherwise              = False

toBytes :: HexString -> B.ByteString
toBytes (HexString bs) = (fst . B16.decode) bs