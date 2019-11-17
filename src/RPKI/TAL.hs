{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.TAL where

import           Control.DeepSeq
import           Control.Monad

import           Data.Data       (Typeable)
import qualified Data.Text       as T

import           GHC.Generics

import           RPKI.Domain
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
  certificateLocations :: [URI],
  publicKeyInfo        :: EncodedBase64
} deriving (Show, Eq, Ord, Typeable, Generic, NFData)

certLocations :: TAL -> [URI]
certLocations PropertiesTAL {..} = [certificateLocation]
certLocations RFC_TAL {..}       = certificateLocations

parseTAL :: T.Text -> Either TALError TAL
parseTAL bs = 
  case (parseAsProperties, parseRFC) of
    (Right t, _)       -> Right t
    (Left _,  Right t) -> Right t
    (Left e1, Left e2) -> Left $ e1 <> e2    
  where
    parseAsProperties = 
      case T.lines bs of 
        [] -> Left $ TALError "Couldn't find newline character."
        lns -> do
          let nonComments = filter (\line -> not $ "#" `T.isPrefixOf` line) lns
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
            Just prefetchStr -> map URI $ T.splitOn "," prefetchStr 

        getMandatory name ps =
            case lookup name ps of
              Nothing -> Left $ TALError $ convert $ "'" <> show name <> "' is not defined."
              Just cl -> Right cl

    parseRFC =      
      case span looksLikeUri $ T.lines bs of
        ([], _) -> Left $ TALError "Empty list of URIs"
        (_, []) -> Left $ TALError "Empty public key info"
        (uris, base64) -> Right $ RFC_TAL {
          certificateLocations = map (URI . T.strip) uris,
          publicKeyInfo = EncodedBase64 $ convert $ 
              T.concat $ filter (not . T.null) $ map T.strip base64
        }

    looksLikeUri s = any (`T.isPrefixOf` s) ["rsync://", "http://", "https://"]


