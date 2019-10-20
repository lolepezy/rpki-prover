{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module RPKI.Store.LMDB where

import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE

import Data.Maybe

import Database.LMDB.Simple
import Database.LMDB.Simple.Extra

import qualified Codec.Serialise as Serialise

import RPKI.Domain
import RPKI.Store.Storage

newtype LMDBStore = LMDBStore { db :: Database Hash BL.ByteString }

instance Storage LMDBStore where
    type TxM mode LMDBStore = Transaction mode

    storeObj (LMDBStore db) ro@(RpkiObject RpkiMeta {..} _) = 
        let b = Serialise.serialise ro
            in put db hash (Just b)

    -- TODO Implement
    getByAKI (LMDBStore db) a = pure []

    getByHash (LMDBStore db) h = do
        r <- get db h
        pure $ Serialise.deserialise <$> r

