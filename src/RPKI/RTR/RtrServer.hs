{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

module RPKI.RTR.RtrServer where

import qualified Data.ByteString.Builder as BB

import Network.Simple.TCP

import RPKI.RTR.Types


write :: Pdu v t -> BB.Builder
write _ = mempty