{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

module RPKI.TAL where

import Data.Bifunctor (first, second)

import           Control.Monad

import           Codec.Serialise

import qualified Data.List                        as List
import           Data.List.NonEmpty               (NonEmpty(..))
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text

import           GHC.Generics

import           RPKI.Domain
import           RPKI.Reporting

import           RPKI.Util                        



-- | Represents Trust Anchor Locator as described here
-- | https://tools.ietf.org/html/rfc8630
-- | Or as so-called RIPE format
-- | (I couldn't find any formal definiteion of the "RIPE format")
-- | 
data TAL = PropertiesTAL {
        caName              :: Maybe Text.Text,
        certificateLocation :: Locations,
        publicKeyInfo       :: EncodedBase64,
        prefetchUris        :: [RpkiURL]
    } 
    | RFC_TAL {
        certificateLocations :: Locations,
        publicKeyInfo        :: EncodedBase64
    } 
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass (Serialise)

certLocations :: TAL -> Locations
certLocations PropertiesTAL {..} = certificateLocation
certLocations RFC_TAL {..}       = certificateLocations

getTaName :: TAL -> TaName
getTaName tal = case tal of 
    PropertiesTAL {..} -> maybe (uri2TaName $ NonEmpty.head certificateLocation) TaName caName
    RFC_TAL {..}       -> uri2TaName $ NonEmpty.head certificateLocations
    where 
        uri2TaName u = 
            let URI t = getURL u
            in TaName t

getTaCertURL :: TAL -> RpkiURL
getTaCertURL PropertiesTAL {..} = NonEmpty.head certificateLocation
getTaCertURL RFC_TAL {..}       = NonEmpty.head certificateLocations

newLocation :: Text.Text -> NonEmpty RpkiURL
newLocation t =  RrdpU (RrdpURL $ URI t) :| []

newLocations :: NonEmpty RpkiURL -> NonEmpty RpkiURL
newLocations = NonEmpty.sortWith urlOrder
  where
    urlOrder u = case u of
        RrdpU _ -> (0 :: Int, u)
        _       -> (1, u)

-- | Parse TAL object from raw text
parseTAL :: Text.Text -> Either TALError TAL
parseTAL bs = 
    case (parseAsProperties, parseRFC) of
        (Right t, _)       -> Right t
        (Left _,  Right t) -> Right t
        (Left (TALError e1), Left (TALError e2)) -> Left $ TALError $ e1 <> " | " <> e2    
    where
        parseAsProperties = 
            case Text.lines bs of 
                [] -> Left $ TALError "Couldn't find newline character."
                lns -> do     
                    -- map each line to a (property name, property value) pair
                    properties <- forM (nonComments lns) $ \line ->
                        case Text.splitOn "=" line of
                            [name, value] -> Right (Text.strip name, Text.strip value)
                            _             -> Left $ TALError [i|Line #{line} doesn't contain '=' sign. |]
                
                    PropertiesTAL <$> 
                        getCaName properties <*>
                        getCertificateLocation properties <*>
                        getPublicKeyInfo properties <*>
                        getPrefetchUris properties      
            where 
                -- Lines that are not comments are the ones not startig with '#'
                nonComments = List.filter $ not . ("#" `Text.isPrefixOf`) . Text.stripStart 

                getCaName ps              = Right $ lookup "ca.name" ps                
                getPublicKeyInfo       ps = EncodedBase64 . convert <$> getMandatory "public.key.info" ps
                getPrefetchUris ps        = first TALError $ 
                    case lookup "prefetch.uris" ps of
                        Nothing          -> Right []
                        Just prefetchStr -> mapM parseRpkiURL $ Text.splitOn "," prefetchStr 

                getCertificateLocation ps = 
                    case lookup propertyName ps of
                        Nothing   -> Left  $ TALError [i|'#{propertyName}' is not defined.|]
                        Just uris -> 
                            case NonEmpty.nonEmpty (Text.splitOn "," uris) of
                                Nothing    -> Left $ TALError [i|Empty list of TA certificate URLs in #{propertyName}|]
                                Just uris' -> first TALError $ second newLocations $ mapM parseRpkiURL uris'
                                    -- 
                    where
                        propertyName = "certificate.location" :: Text.Text

                getMandatory name ps =
                    case lookup name ps of
                        Nothing -> Left $ TALError [i|'#{name}' is not defined.|]
                        Just cl -> Right cl

        parseRFC =      
            case List.span looksLikeUri $ Text.lines bs of        
                (_, [])        -> Left $ TALError "Empty public key info"
                (uris, base64) ->
                    case NonEmpty.nonEmpty uris of
                        Nothing    -> Left $ TALError "Empty list of URIs"
                        Just uris' -> do 
                            locations <- first TALError $ 
                                            second newLocations $ 
                                            mapM parseRpkiURL $ 
                                            NonEmpty.map Text.strip uris'
                            pure $ RFC_TAL {
                                certificateLocations = locations,
                                publicKeyInfo = EncodedBase64 $ convert $ 
                                    Text.concat $ filter (not . Text.null) $ map Text.strip base64
                            }
            where 
                looksLikeUri s = any (`Text.isPrefixOf` s) ["rsync://", "http://", "https://"]
