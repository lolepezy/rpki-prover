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
import qualified Data.Text                  as Text

import           Data.X509

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Time
import           RPKI.Parse.Internal.Common
import qualified RPKI.Util                  as U


parseCrl :: BS.ByteString -> PureValidatorT CrlObject
parseCrl bs = do
    -- pureError $ parseErr $ "Couldn't parse IP address extension: " <> Text.pack (show e)
    asns                   <- fromEither $ first (parseErr . U.fmtGen) $ decodeASN1' BER bs
    (extensions, signCrlF) <- fromEither $ first (parseErr . U.convert) $ runParseASN1 getCrl asns      
    akiBS <- case extVal extensions id_authorityKeyId of
                Nothing -> pureError $ parseErr "No AKI in CRL"
                Just a  -> pure a

    aki' <- case decodeASN1' BER akiBS of
                Left e -> pureError $ parseErr $ "Unknown AKI format: " <> U.fmtGen e
                Right [Start Sequence, Other Context 0 ki, End Sequence] -> pure ki
                Right s -> pureError $ parseErr $ "Unknown AKI format: " <> U.fmtGen s
    
    crlNumberBS :: BS.ByteString  <- case extVal extensions id_crlNumber of
                Nothing -> pureError $ parseErr "No CRL number in CRL"
                Just n  -> pure n

    numberAsns <- fromEither $ first (parseErr . U.fmtGen) $ decodeASN1' BER crlNumberBS
    crlNumber' <- fromEither $ first (parseErr . U.convert) $ 
                    runParseASN1 (getInteger pure "Wrong CRL number") numberAsns

    case makeSerial crlNumber' of 
        Left e       -> pureError $ parseErr $ Text.pack e
        Right crlNum -> pure $ newCrl         
                            (AKI $ mkKI aki') 
                            (U.sha256s bs) 
                            (signCrlF crlNum)        
    where          
        getCrl = onNextContainer Sequence $ do
            (asns, z) <- getNextContainerMaybe Sequence >>= \case 
                Nothing   -> throwParseError "Invalid CRL format"
                Just asns -> 
                    case runParseASN1 getCrlContent asns of
                        Left e  -> throwParseError $ "Invalid CRL format: " <> e
                        Right c -> pure (asns, c)

            let (thisUpdate, nextUpdate, extensions, revoked) = z

            signatureId :: SignatureALG <- getObject
            signatureVal <- parseSignature

            let encoded = encodeASN1' DER $ [Start Sequence] <> asns <> [End Sequence]

            let mkSignCRL crlNumber' = SignCRL 
                        (Instant thisUpdate)
                        (Instant <$> nextUpdate)
                        (SignatureAlgorithmIdentifier signatureId) 
                        signatureVal (toShortBS encoded) 
                        crlNumber' 
                        revoked
            pure (extensions, mkSignCRL)            
        
        getCrlContent = do        
            -- This is copy-pasted from the Data.X509.CRL to fix getRevokedCertificates 
            -- which should be more flexible.            
            _ :: Integer           <- getNext >>= getVersion
            _ :: SignatureALG      <- getObject
            _ :: DistinguishedName <- getObject
            thisUpdate      <- getNext >>= getThisUpdate
            nextUpdate      <- getNextUpdate
            revoked         <- getRevokedSerials
            _ :: Extensions <- getObject        
            extensions      <- onNextContainer (Container Context 0) $ 
                                    onNextContainer Sequence $ getMany getObject             
            pure (thisUpdate, nextUpdate, extensions, revoked)
            where 
                getVersion (IntVal v) = pure $! fromIntegral v
                getVersion _          = throwParseError "Unexpected type for version"

                getThisUpdate (ASN1Time _ t _) = pure t
                getThisUpdate t                = throwParseError $ "Bad this update format, expecting time: " <> show t

                getNextUpdate = getNextMaybe $ \case 
                    (ASN1Time _ tnext _) -> Just tnext
                    _                    -> Nothing

                -- TODO This is heavy and eats a lot of heap for long revocation lists
                getRevokedSerials = 
                    maybe Set.empty Set.fromList <$> 
                        onNextContainerMaybe Sequence (getMany getCrlSerial)
                    where
                        getCrlSerial = onNextContainer Sequence $ 
                            replicateM 2 getNext >>= \case 
                                [IntVal serial', _] -> 
                                    case makeSerial serial' of 
                                        Left e  -> throwParseError e
                                        Right s -> pure s
                                s                  -> throwParseError $ "That's not a serial: " <> show s
                                
                                
                            


                    



