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

import           Control.Monad.Primitive    (PrimMonad (..), stToPrim)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (isSpace)
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.String.Utils          (strip)
import qualified Data.Text                  as T
import qualified Network.Wreq               as WR

import           Data.Hex                   (unhex)
import           Data.Word

import           Data.IORef
import           Text.Read                  (readMaybe)

import           Xeno.SAX                   as X

import           RPKI.Domain
import           RPKI.RRDP.Types
import           RPKI.Util                  (convert)


-- Download the notification
getNotifications :: URI -> IO (Either RrdpError Notification)
getNotifications (URI uri) = do
    r <- WR.get $ T.unpack uri
    pure $ parseNotification $ r ^. WR.responseBody


parseNotification :: BL.ByteString -> Either RrdpError Notification
parseNotification bs = runST $ do
    deltas       <- newSTRef []
    snapshotUri  <- newSTRef Nothing
    snapshotHash <- newSTRef Nothing
    serial       <- newSTRef Nothing
    sessionId    <- newSTRef Nothing

    let parse = parseXml (BL.toStrict bs)
            (\case
                ("notification", attributes) -> do
                    forAttribute attributes "session_id" NoSessionId $
                        \v -> lift $ writeSTRef sessionId $ Just $ SessionId v
                    forAttribute attributes "serial" NoSerial $
                        \v -> parseSerial v (lift . writeSTRef serial . Just)
                ("snapshot", attributes) -> do
                    forAttribute attributes "hash" NoSnapshotHash $
                        \v  -> case makeHash v of
                            Nothing   -> throwE $ BadHash v
                            Just hash -> lift $ writeSTRef snapshotHash $ Just hash
                    forAttribute attributes "haurish" NoSnapshotURI $
                        \v  -> lift $ writeSTRef snapshotUri $ Just $ URI $ convert v
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

    let notification = Notification <$>
                        (valuesOrError sessionId NoSessionId) <*>
                        (valuesOrError serial NoSerial ) <*>
                        (valuesOrError snapshotUri NoSnapshotURI ) <*>
                        (valuesOrError snapshotHash NoSnapshotHash ) <*>
                        (lift . readSTRef) deltas

    runExceptT (parse >> notification)


{-
    <snapshot version="1" session_id="4dcbcbf2-8f3e-4b1d-a626-7a5d3b39d0db" serial="181"
    <publish uri="">text</publish>
-}

parseSnapshot :: BL.ByteString -> Either RrdpError Snapshot
parseSnapshot bs = runST $ do
    publishes <- newSTRef []
    sessionId <- newSTRef Nothing
    serial    <- newSTRef Nothing
    version   <- newSTRef Nothing

    let parse = parseXml (BL.toStrict bs)
            (\case
                ("snaphot", attributes) -> do
                    forAttribute attributes "session_id" NoSessionId $
                        \v -> lift $ writeSTRef sessionId $ Just $ SessionId v
                    forAttribute attributes "serial" NoSerial $
                        \v -> parseSerial v (lift . writeSTRef serial . Just)
                    forAttribute attributes "version" NoSerial $
                        \v -> case parseInteger v of
                            Nothing -> throwE NoVersion
                            Just v'
                                | v' == 1    -> lift $ writeSTRef version  (Just $ Version v')
                                | otherwise -> throwE $ BadVersion v

                ("publish", attributes) -> do
                    uri <- forAttribute attributes "uri" NoPublishURI (lift . pure)
                    lift $ modifySTRef publishes $ \ps -> (uri, Nothing) : ps
            )
            (\base64 ->
                case toContent base64 of
                    Left e        -> throwE $ BadBase64 $ convert e
                    Right content -> do
                        (lift . readSTRef) publishes >>= \case
                            []            -> throwE $ BadBase64 ""
                            (uri, _) : ps -> lift $ writeSTRef publishes $ (uri, Just content) : ps
            )

    let snapshotPublishes ps =
            [ SnapshotPublish (URI $ convert uri) content | (uri, Just content) <- reverse ps ]
    let snapshot = Snapshot <$>
                    (valuesOrError version NoVersion) <*>
                    (valuesOrError sessionId NoSessionId) <*>
                    (valuesOrError serial NoSerial) <*>
                    (snapshotPublishes <$> (lift . readSTRef) publishes)

    runExceptT (parse >> snapshot)


parseInteger :: B.ByteString -> Maybe Integer
parseInteger bs = readMaybe $ convert bs

forAttribute :: Monad m =>
                M.Map B.ByteString B.ByteString ->
                B.ByteString ->
                RrdpError ->
                (B.ByteString -> ExceptT RrdpError m v) ->
                ExceptT RrdpError m v
forAttribute as name err f = case M.lookup name as of
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

makeHash bs = (Hash SHA256 . toBytes) <$> hexString bs

type Element = (B.ByteString, M.Map B.ByteString B.ByteString)

parseXml :: (MonadTrans t, PrimMonad pm, Monad (t pm)) =>
            B.ByteString ->
            (Element -> t pm ()) ->
            (B.ByteString -> t pm ()) ->
            t pm ()
parseXml bs onElement onText = do
    element <- lift $ stToPrim $ newSTRef ("" :: B.ByteString, M.empty)
    X.process
        (\elemName -> lift $ stToPrim $ modifySTRef' element (\(_, as) -> (elemName, as)))
        (\name value -> lift $ stToPrim $ modifySTRef' element (\(n, as) -> (n, M.insert name value as)))
        (\elemName -> (lift $ stToPrim $ readSTRef element) >>= onElement)
        onText
        nothing
        nothing
        bs
    where nothing = (\_ -> lift $ pure ())


-- Parsing HEX stuff
newtype HexString = HexString B.ByteString deriving ( Show, Eq, Ord )

hexString :: B.ByteString -> Maybe HexString
hexString bs = HexString <$> unhex bs

toBytes :: HexString -> B.ByteString
toBytes (HexString bs) = (fst . B16.decode) bs

toContent :: B.ByteString -> Either String Content
toContent bs = Content <$> (B64.decode trimmed)
    where
        trimmed = convert $ strip $ convert bs
