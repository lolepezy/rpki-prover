{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.Fetch.ErikRelay where

import           Control.Concurrent              as Conc
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens hiding (indices, Indexable)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Generics.Product.Typed

import qualified Data.List.NonEmpty          as NonEmpty

import           Data.Text                        (Text)
import           Data.Data
import           Data.Foldable                   (for_)
import           Data.Maybe 
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map            
import qualified Data.Map.Monoidal.Strict        as MonoidalMap     
import           Data.String.Interpolate.IsString

import           GHC.Generics

import           Time.Types

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Parse.Parse
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Repository
import           RPKI.RRDP.Types
import           RPKI.Store.Base.Storage
import           RPKI.Time
import           RPKI.Parallel
import           RPKI.Util                       
import           RPKI.Rsync
import           RPKI.Fetch.Http
import           RPKI.TAL
import           RPKI.RRDP.RrdpFetch

fetchErik :: MonadIO m => Config -> URI -> FQDN -> ValidatorT m ErikIndex
fetchErik config uri (FQDN fqdn) = do
    (indexBs, _, httpStatus, _ignoreEtag) <- fetchIndex
    index <- vHoist $ parseErikIndex indexBs



    appError $ UnspecifiedE "options" "ErikRelay fetcher is not implemented yet"
  where 
    fetchIndex = do 
        let tmpDir = configValue $ config ^. #tmpDirectory
        let maxSize = config ^. typed @RrdpConf . #maxSize
        liftIO $ downloadToBS tmpDir indexUri Nothing maxSize            

    indexUri = URI [i|#{uri}/.well-known/erik/index/#{fqdn}|]
        
