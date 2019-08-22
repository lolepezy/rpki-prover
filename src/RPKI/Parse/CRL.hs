{-# LANGUAGE OverloadedStrings   #-}

module RPKI.Parse.CRL where

import qualified Data.ByteString          as B

import           Data.ASN1.Types
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Parse

import Data.X509

import           RPKI.Domain
import qualified RPKI.Util as U
import           RPKI.Parse.Common
import           RPKI.SignTypes


parseCrl :: B.ByteString -> ParseResult (URI -> (CrlMeta, CrlObject))
parseCrl bs = do
  asns                    <- first (fmtErr . show) $ decodeASN1' BER bs
  signCrl@(SignCRL c _ _) <- first fmtErr $ runParseASN1 getCrl asns  
  exts <- case crlExtensions c of
            Extensions Nothing           -> Left $ fmtErr "No CRL extensions"
            Extensions (Just extensions) -> Right extensions
  aki' <- case extVal exts id_authorityKeyId of
            Nothing -> Left $ fmtErr "No AKI in CRL"
            Just a  -> Right a

  crlNumberBS :: B.ByteString <- case extVal exts id_crlNumber of
            Nothing -> Left $ fmtErr "No CRL number in CRL"
            Just n  -> Right n

  number <- first (fmtErr . show) $ decodeASN1' BER crlNumberBS
  n <- first fmtErr $ runParseASN1 (getInteger pure "Wrong CRL number") number

  let meta location = CrlMeta {
      locations = location :| [],
      aki = AKI $ KI aki',
      hash = U.sha256s bs,
      crlNumber = n
    }
  pure $ \location -> (meta location, CrlObject signCrl)
  where  
    getCrl = onNextContainer Sequence $ do
      crl' <- onNextContainer Sequence $ do        
        x509Crl    <- parseX509CRL
        extensions <- onNextContainer (Container Context 0) $ 
                        onNextContainer Sequence $ getMany $ getObject 
        pure $ x509Crl { crlExtensions = Extensions (Just extensions) }
      SignCRL crl' <$> 
        (SignatureAlgorithmIdentifier <$> getObject) <*> 
        parseSignature

    parseX509CRL = do
        CRL <$> (getNext >>= getVersion)
            <*> getObject
            <*> getObject
            <*> (getNext >>= getThisUpdate)
            <*> getNextUpdate
            <*> getRevokedCertificates
            <*> getObject
      where getVersion (IntVal v) = pure $ fromIntegral v
            getVersion _          = throwParseError "Unexpected type for version"

            getThisUpdate (ASN1Time _ t _) = pure t
            getThisUpdate t                = throwParseError $ "Bad this update format, expecting time" <> show t

            getNextUpdate = getNextMaybe $ \case 
              (ASN1Time _ tnext _) -> Just tnext
              _                    -> Nothing

            getRevokedCertificates = 
              onNextContainerMaybe Sequence (getMany getObject) >>= \case
                Nothing -> pure []
                Just rc -> pure rc




