{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}

module RPKI.Http.UI where

import Control.Monad

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Short       as BSS

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set

import Data.Maybe (maybeToList)

import           Data.ByteArray              (convert)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)

import           GHC.Generics                (Generic)

import qualified Crypto.PubKey.Curve25519    as C25519
import qualified Crypto.PubKey.Curve448      as C448
import           Crypto.PubKey.DSA           (Params (..), PublicKey (..))
import           Crypto.PubKey.ECC.Types
import qualified Crypto.PubKey.Ed25519       as E25519
import qualified Crypto.PubKey.Ed448         as E448
import           Crypto.PubKey.RSA.Types     (PublicKey (..))
import           Data.ASN1.BitArray
import           Data.ASN1.Types
import qualified Data.ByteString.Base16      as Hex
import qualified Data.ByteString.Base16.Lazy as HexLazy
import           Data.Hourglass
import           Data.X509                   as X509

import           Servant.API
import           Servant.CSV.Cassava
import           Servant.HTML.Blaze

import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept(..))

import           RPKI.Domain                 as Domain
import           RPKI.Config
import           RPKI.CommonTypes

import           RPKI.Reporting
import           RPKI.Resources.IntervalSet
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storable

import           RPKI.Store.Database
import           RPKI.Time
import qualified RPKI.Util                   as U


import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal as I
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze
import RPKI.Http.Types
import RPKI.Http.Messages


instance ToMarkup [ValidationResult] where
  toMarkup = validaionResultsHtml

type UI = "ui" :> "validation-results.html" :> Get '[HTML] Html


withUI :: Html -> Html 
withUI include =
    H.html $ do
        H.head $ do
            link ! rel "stylesheet" ! href "/styles.css"
            script ! src "/jquery.min.js" $ mempty
            script ! src "/functions.js" ! type_ "text/javascript" $ mempty
        H.body include


validaionResultsHtml :: [ValidationResult] -> Html
validaionResultsHtml result = 
    H.table $ do 
        thead $ tr $ do 
            th $ H.span $ toHtml ("Problem" :: Text)
            th $ H.span $ toHtml ("URL" :: Text)        
        forM_ (Map.toList $ groupByTa result) $ \(ta, vrs) -> do 
            tbody ! A.class_ "labels" $ do 
                tr $ td ! colspan "2" $ do
                    let taText = textValue ta
                    H.label ! A.for taText $ toHtml ta
                    input ! A.type_ "checkbox" ! name taText ! A.id taText ! I.dataAttribute "toggle" "toggle"
            tbody ! class_ "hide" $ do
                forM_ vrs vrHtml

  where
      vrHtml ValidationResult{..} = do 
        let url = Prelude.head context 
        forM_ problems $ \p -> do                         
            tr $ do 
                let (marker, problem) = 
                        case p of 
                            VErr p             -> ("red-dot",    p)                                        
                            VWarn (VWarning p) -> ("yellow-dot", p)
                td $ H.span $ do 
                    H.span ! A.class_ marker $ ""
                    space >> space
                    toHtml $ toMessage problem

                -- TODO This is pretty ugly, find a better way to get a proper URL
                let link = "/api/object?uri=" <> url
                td $ H.a ! A.href (textValue link) $ toHtml url


groupByTa :: [ValidationResult] -> Map Text [ValidationResult]
groupByTa vrs = 
    Map.fromListWith (<>) 
    $ [ (ta, [vr]) 
            | vr@(ValidationResult {..}) <- vrs, 
              ta <- lastOne context ]    
  where
    lastOne [] = []
    lastOne xs = [last xs]

space = preEscapedToMarkup ("&nbsp;" :: Text)