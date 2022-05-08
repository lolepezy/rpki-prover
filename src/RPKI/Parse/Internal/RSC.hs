{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Parse.Internal.RSC where

import qualified Data.ByteString as BS  

import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse

import Data.Bifunctor
import Data.String.Interpolate.IsString

import Data.Tuple.Strict

import RPKI.Domain 
import RPKI.Resources.Types
import RPKI.Parse.Internal.Common
import RPKI.Parse.Internal.SignedObject 

import RPKI.Util
import RPKI.Resources.Resources as R
import RPKI.Resources.IntervalSet as IS
import Data.Either


-- | Parse RSC, https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-rsc/
-- 
parseRSC :: BS.ByteString -> ParseResult RscObject
parseRSC bs = do    
    asns      <- first (fmtErr . show) $ decodeASN1' BER bs      
    signedRsc <- first fmtErr $ runParseASN1 (parseSignedObject $ parseSignedContent parseRsc') asns
    hash' <- getMetaFromSigned signedRsc bs
    pure $ newCMSObject hash' (CMS signedRsc)
  where     
    parseRsc' = onNextContainer Sequence $ do        
        ((v4, v6), asn) <- parseResourceSet        
        digestAlgorithm <- onNextContainer Sequence getDigest
        checkList       <- parseCheckList
        let rscResources = PrefixesAndAsns v4 v6 asn
        pure RSC {..}

    parseResourceSet = 
        onNextContainer Sequence $ do
            let f parse asn1 = either (throwParseError . show) pure $ runParseASN1 parse asn1

            asns <- getNextContainerMaybe (Container Context 0) >>= \case 
                Nothing   -> pure IS.empty
                Just asn1 -> f parseAsns asn1
            
            ips <- getNextContainerMaybe (Container Context 1) >>= \case 
                    Nothing   -> pure (IS.empty, IS.empty)
                    Just asn1 -> f parseIps asn1                        

            pure (ips, asns)

    parseCheckList = 
        onNextContainer Sequence $ 
            getMany $ 
                onNextContainer Sequence $ 
                    getNext >>= \case 
                        ASN1String (ASN1CharacterString IA5 filename) ->
                            getNext >>= \case
                                OctetString os -> pure $ T2 (Just $ convert filename) (mkHash os)
                                other          -> throwParseError $ "Unexpected checklist item: " ++ show other
                        OctetString os         -> pure $ T2 Nothing (mkHash os)
                        other                  -> throwParseError $ "Unexpected checklist item: " ++ show other
    
    parseIps = do 
        z <- onNextContainer Sequence $ do                                        
                getMany $ 
                    onNextContainer Sequence $ do                         
                        getAddressFamily "Expected an address family here" >>= \case
                            Right Ipv4F -> Left  <$> ipv4Address
                            Right Ipv6F -> Right <$> ipv6Address
                            Left af     -> throwParseError $ "Unsupported address family " <> show af                
        pure $ bimap 
            (IS.fromList . mconcat) 
            (IS.fromList . mconcat) 
            $ partitionEithers z        

    parseAsns = IS.fromList <$> onNextContainer Sequence (getMany asOrRange)
        
