{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.Internal.CRL where
    
import           Control.Monad

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Parse
import           Data.ASN1.Types
import           Data.Bifunctor             (first)
import qualified Data.ByteString            as BS
import qualified Data.Set                   as Set

import           Data.X509

import           RPKI.Domain
import           RPKI.Time
import           RPKI.Parse.Internal.Common
import qualified RPKI.Util                  as U


parseCrl :: BS.ByteString -> ParseResult (URI -> CrlObject)
parseCrl bs = do
    asns                <- first (fmtErr . show) $ decodeASN1' BER bs
    (extensions, signCrlF) <- first fmtErr $ runParseASN1 getCrl asns      
    akiBS <- case extVal extensions id_authorityKeyId of
                Nothing -> Left $ fmtErr "No AKI in CRL"
                Just a  -> Right a

    aki' <- case decodeASN1' BER akiBS of
                Left e -> Left $ fmtErr $ "Unknown AKI format: " <> show e
                Right [Start Sequence, Other Context 0 ki, End Sequence] -> Right ki
                Right s -> Left $ fmtErr $ "Unknown AKI format: " <> show s
    
    crlNumberBS :: BS.ByteString  <- case extVal extensions id_crlNumber of
                Nothing -> Left $ fmtErr "No CRL number in CRL"
                Just n  -> Right n

    numberAsns <- first (fmtErr . show) $ decodeASN1' BER crlNumberBS
    crlNumber' <- first fmtErr $ runParseASN1 (getInteger pure "Wrong CRL number") numberAsns

    pure $ \location -> makeCrl 
        location 
        (AKI $ mkKI aki') 
        (U.sha256s bs) 
        (signCrlF crlNumber' )        
    where          
        getCrl = onNextContainer Sequence $ do
            (asns, z) <- getNextContainerMaybe Sequence >>= \case 
                Nothing   -> throwParseError "Invalid CRL format"
                Just asns -> 
                    case runParseASN1 getCrlContent asns of
                        Left e  -> throwParseError $ "Invalid CRL format: " <> e
                        Right c -> pure (asns, c)

            let (thisUpdate, nextUpdate, extensions, revokedSerials) = z

            signatureId :: SignatureALG <- getObject
            signatureVal <- parseSignature

            let encoded = encodeASN1' DER $ [Start Sequence] <> asns <> [End Sequence]

            let mkSignCRL crlNumber' = SignCRL 
                        (Instant thisUpdate)
                        (Instant <$> nextUpdate)
                        (SignatureAlgorithmIdentifier signatureId) 
                        signatureVal encoded crlNumber' revokedSerials
            pure (extensions, mkSignCRL)            
        
        getCrlContent = do        
            -- This is copy-pasted from the Data.X509.CRL to fix getRevokedCertificates 
            -- which should be more flexible.            
            _ :: Integer           <- getNext >>= getVersion
            _ :: SignatureALG      <- getObject
            _ :: DistinguishedName <- getObject
            thisUpdate      <- getNext >>= getThisUpdate
            nextUpdate      <- getNextUpdate
            revokedSerials  <- getRevokedSerials
            _ :: Extensions <- getObject        
            extensions      <- onNextContainer (Container Context 0) $ 
                                    onNextContainer Sequence $ getMany getObject             
            pure (thisUpdate, nextUpdate, extensions, revokedSerials)
            where 
                getVersion (IntVal v) = pure $! fromIntegral v
                getVersion _          = throwParseError "Unexpected type for version"

                getThisUpdate (ASN1Time _ t _) = pure t
                getThisUpdate t                = throwParseError $ "Bad this update format, expecting time: " <> show t

                getNextUpdate = getNextMaybe $ \case 
                    (ASN1Time _ tnext _) -> Just tnext
                    _                    -> Nothing

                -- TODO This is heavy and eats a lot o heap for long revocation lists
                getRevokedSerials = 
                    onNextContainerMaybe Sequence (getMany getCrlSerial) >>= \case
                        Nothing -> pure Set.empty
                        Just rc -> pure $! Set.fromList rc
                    where
                        getCrlSerial = onNextContainer Sequence $ 
                            replicateM 2 getNext >>= \case 
                                [IntVal serial, _] -> pure $! Serial serial                            
                                s                  -> throwParseError $ "That's not a serial: " <> show s
                                
                                
                            


                    



