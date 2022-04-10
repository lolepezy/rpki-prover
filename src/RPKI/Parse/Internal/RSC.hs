{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Parse.Internal.RSC where

import qualified Data.ByteString as BS  

import Control.Monad
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse
import Data.ASN1.BitArray

import Data.Bifunctor
import Data.String.Interpolate.IsString

import Data.Tuple.Strict

import RPKI.Domain 
import RPKI.Resources.Types
import RPKI.Parse.Internal.Common
import RPKI.Parse.Internal.SignedObject 

import RPKI.Util

-- | Parse RSC, https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-rsc/
-- 
-- parseRCS :: BS.ByteString -> ParseResult RscObject
-- parseRCS bs = do    
--     asns      <- first (fmtErr . show) $ decodeASN1' BER bs      
--     signedRsc <- first fmtErr $ runParseASN1 (parseSignedObject $ parseSignedContent parseRsc') asns
--     hash' <- getMetaFromSigned signedRsc bs
--     pure $ newCMSObject hash' (CMS signedRsc)
--   where     
--     parseRsc' = onNextContainer Sequence $ do
--         -- resources       <- parseResourceSet
--         -- digestAlgorithm <- onNextContainer Sequence getDigest
--         -- checkList       <- parseCheckList
--         pure RCS {..}

    -- parseResourceSet = 
    --     onNextContainer Sequence $ 
    --         onNextContainer (Container Context 1) $ 
    --             onNextContainer Sequence $ 
    --                 getMany $
    --                     getAddressFamily "Expected an address family here" >>= \case 
    --                         Right Ipv4F -> getPrefix Ipv4F
    --                         Right Ipv6F -> getRoa asId Ipv6F
    --                         Left af     -> throwParseError $ "Unsupported address family: " ++ show af

    -- parseCheckList = 
    --     onNextContainer Sequence $ 
    --         getMany $ 
    --             onNextContainer Sequence $ 
    --                 getNext >>= \case 
    --                     ASN1String (ASN1CharacterString IA5 filename) ->
    --                         getNext >>= \case
    --                             OctetString os -> pure $ T2 (Just $ convert filename) (mkHash os)
    --                             other          -> throwParseError $ "Unexpected checklist item: " ++ show other
    --                     OctetString os         -> pure $ T2 Nothing (mkHash os)
    --                     other                  -> throwParseError $ "Unexpected checklist item: " ++ show other



{-
    Start Sequence,
        Start Sequence,
            Start (Container Context 1),
                Start Sequence,
                    Start Sequence,
                        OctetString \"\\NUL\\STX\",
                        BitString (BitArray 48 \" \\SOH\\ACK| \\140\"),
                    End Sequence,
                End Sequence,
            End (Container Context 1),
        End Sequence,
        Start Sequence,
            OID [2,16,840,1,101,3,4,2,1],
        End Sequence,
        Start Sequence,
            Start Sequence,
                ASN1String (ASN1CharacterString {characterEncoding = IA5, getCharacterStringRawData = \"b42_ipv6_loa.png\"}),
                OctetString \"\\149\\SYN\\221d\\190|\\ETB%\\185\\252\\161\\ETB\\DC2\\SOX\\232\\216B\\165 hs9\\155=\\223\\252\\145\\196\\182\\172\\240\",                
            End Sequence,
            Start Sequence,
                OctetString \"\\n\\225\\&9G\\\"\\NUL\\\\\\217/Lj\\160$\\213\\214\\179\\226\\230}b\\159\\DC1r\\r\\148x\\166\\&3\\161\\ETB\\161\\199\",
            End Sequence,
        End Sequence,
    End Sequence
-}