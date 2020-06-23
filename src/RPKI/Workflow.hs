{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Workflow where

import           Control.Concurrent.STM
import           Control.Monad

import           Data.String.Interpolate.IsString

import           GHC.Generics

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Database
import           RPKI.TopDown
import           RPKI.Version

import           RPKI.AppContext
import           RPKI.Store.Base.Storage
import           RPKI.TAL


data FlowTask
  = ValidateTA WorldVersion
  | ValidateTACert TAL WorldVersion
  | GC WorldVersion

-- runCacheGC = do
data TaskProcessor = TaskProcessor { 
    tasks :: ClosableQueue FlowTask 
}



executeTask :: Storage s => 
                AppContext s -> FlowTask -> TaName -> IO ()
executeTask appContext@AppContext {..} task taName'@(TaName taNameText) =
    case task of 
        ValidateTACert tal worldVersion -> do
            let context = vContext $ URI taNameText
            (r, validations) <- runValidatorT context $ validateTACertificateFromTAL appContext tal 
            writeVResult appContext validations worldVersion
            case r of
                Left e -> 
                    logError_ logger [i|Error updating TA certificate for #{taNameText}, e = #{e}.|]
                Right (_, _, Updated) ->
                    -- TODO Subnmit the task here instead of executing it directly
                    void $ validateTA appContext taName' worldVersion
                Right (_, _, Existing) ->
                    -- Nothing to do, TA certificate is the same, 
                    -- so repository re-validation will kick in on it's own time
                    pure ()
        ValidateTA worldVersion -> do
            void $ validateTA appContext taName' worldVersion
        

submitTask :: FlowTask -> TaskProcessor -> IO ()
submitTask task (TaskProcessor queue) = 
    atomically $ writeCQueue queue task


    