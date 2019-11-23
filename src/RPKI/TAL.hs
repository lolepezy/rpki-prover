{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.TAL where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Except

import           Data.Data       (Typeable)
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.List as L
import qualified Data.Text       as T

import           GHC.Generics
import           Data.Has

import           RPKI.Domain
import           RPKI.Parse.Parse
import           RPKI.Rsync
import           RPKI.AppMonad
import           RPKI.Logging
import           RPKI.Util       (convert)


-- | Represents Trust Anchor Locator as described here
-- | https://tools.ietf.org/html/rfc8630
-- | Or as so-called RIPE format
-- | (I couldn't find any formal definiteion of the "RIPE format")
-- | 
data TAL = PropertiesTAL {
  caName              :: Maybe T.Text,
  certificateLocation :: URI,
  publicKeyInfo       :: EncodedBase64,
  prefetchUris        :: [URI]
} | RFC_TAL {
  certificateLocations :: NonEmpty URI,
  publicKeyInfo        :: EncodedBase64
} deriving (Show, Eq, Ord, Typeable, Generic, NFData)

certLocations :: TAL -> NonEmpty URI
certLocations PropertiesTAL {..} = certificateLocation :| []
certLocations RFC_TAL {..}       = certificateLocations


-- | Parse TAL object from raw text
parseTAL :: T.Text -> Either TALError TAL
parseTAL bs = 
  case (parseAsProperties, parseRFC) of
    (Right t, _)       -> Right t
    (Left _,  Right t) -> Right t
    (Left (TALError e1), Left (TALError e2)) -> Left $ TALError $ e1 <> " | " <> e2    
  where
    parseAsProperties = 
      case T.lines bs of 
        [] -> Left $ TALError "Couldn't find newline character."
        lns -> do
          let nonComments = L.filter (\line -> not $ "#" `T.isPrefixOf` line) lns
          properties <- forM nonComments $ \line ->
            case T.splitOn "=" line of
              [name, value] -> Right (T.strip name, T.strip value)
              _             -> Left $ TALError $ convert $ "Line " <> show line <> " doesn't contain '=' sign."
          
          PropertiesTAL <$> 
            getCaName properties <*>
            getCertificateLocation properties <*>
            getPublicKeyInfo properties <*>
            getPrefetchUris properties      
      where 
        getCaName ps              = Right $ lookup "ca.name" ps
        getCertificateLocation ps = URI     <$> getMandatory "certificate.location" ps
        getPublicKeyInfo       ps = EncodedBase64 . convert <$> getMandatory "public.key.info" ps
        getPrefetchUris ps        = Right $
          case lookup "prefetch.uris" ps of
            Nothing          -> []
            Just prefetchStr -> L.map URI $ T.splitOn "," prefetchStr 

        getMandatory name ps =
            case lookup name ps of
              Nothing -> Left $ TALError $ convert $ "'" <> show name <> "' is not defined."
              Just cl -> Right cl

    parseRFC =      
      case L.span looksLikeUri $ T.lines bs of        
        (_, []) -> Left $ TALError "Empty public key info"
        (uris, base64) ->
          case NE.nonEmpty uris of
            Nothing    -> Left $ TALError "Empty list of URIs"
            Just uris' -> Right $ RFC_TAL {
              certificateLocations = NE.map (URI . T.strip) uris',
              publicKeyInfo = EncodedBase64 $ convert $ 
                  T.concat $ filter (not . T.null) $ map T.strip base64
            }

    looksLikeUri s = any (`T.isPrefixOf` s) ["rsync://", "http://", "https://"]


-- | Create a persistable TA object from a parsed TAL.
-- | It means fetch the TA certificate, validate it and 
createTAFromTAL :: (Has RsyncConf conf, Has AppLogger conf) => 
                  TAL -> 
                  ValidatorT conf IO TA
createTAFromTAL tal = do  
  logger :: AppLogger    <- asks getter
  rsyncConf :: RsyncConf <- asks getter
  (u, object) <- fetchTACertificate logger rsyncConf 
  puteToValidatorT $ validateTACert tal u object
  where
    fetchTACertificate logger rsyncConf = lift $ go $ NE.toList $ certLocations tal
      where
        go []         = throwE $ TAL_E $ TALError "No certificate location could be fetched."
        go (u : uris) = catchE ((u,) <$> rsync) $ \e -> do          
            lift2 $ logError_ logger $ convert $ "Failed to fetch " <> show u <> ": " <> show e
            go uris
            where
              rsync = runReaderT (rsyncFile u checkForWeirdSizes) (logger, rsyncConf)

      
    checkForWeirdSizes :: B.ByteString -> Maybe ValidationError
    checkForWeirdSizes bs = 
      case () of _
                  | len < 10             -> Just $ TACertificateIsTooSmall len
                  | len > 10 * 1000*1000 -> Just $ TACertificateIsTooBig len
                  | otherwise            -> Nothing
      where len = B.length bs


-- | 
validateTACert :: TAL -> URI -> RpkiObject -> PureValidator TA
validateTACert tal u (RpkiObject RpkiMeta {..}  (CerRO (CerObject rc@(ResourceCert taCert)))) =
    let 
      spki = subjectPublicKeyInfo $ getX509Cert $ withRFC taCert certX509
    in 
      if publicKeyInfo tal == spki 
      then pure $ TA {
            taName = TaName $ case caName tal of
                        Nothing -> unURI u
                        Just ca -> ca,
            taCertificate = rc,
            taUri = u,
            taSpki = SPKI spki
          }                
      else 
        pureError $ SPKIMismatch (publicKeyInfo tal) spki

validateTACert _ _ _ = pureError UnknownObjectAsTACert