module Main where

import Colog

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted

import Data.Bifunctor
import qualified Data.ByteString                as BS

import qualified Network.Wai.Handler.Warp as Warp

import           RPKI.AppMonad
import           RPKI.Errors
import           RPKI.Domain
import           RPKI.Execution
import           RPKI.Logging
import           RPKI.TopDown
import           RPKI.Config
import           RPKI.TAL
import           RPKI.Http.Server
import           RPKI.Util (fmtEx, convert)
import           RPKI.Store.Util

--
servantAPI :: IO ()
servantAPI = pure ()

main :: IO ()
main = do
    -- load config file and apply command line options
    -- initialise environment and LMDB
    concurrently
        runHttpApi
        runValidatorApp
    return ()


runValidatorApp :: IO ()
runValidatorApp = do 
    lmdbEnv  <- mkLmdb "./data" 1000
    database <- createDatabase lmdbEnv
    appContext <- createAppContext
    let tals = [ "afrinic.tal", "apnic.tal", "arin.tal", "lacnic.tal", "ripe.tal" ]
    -- let tals = [ "ripe.tal" ]

    as <- forM tals $ \tal -> async $
        runValidatorT (vContext $ URI $ convert tal) $ do
            t <- fromTry (RsyncE . FileReadError . fmtEx) $             
                BS.readFile $ "/Users/mpuzanov/.rpki-cache/tals/" <> tal
            talContent <- vHoist $ fromEither $ first TAL_E $ parseTAL $ convert t                        
            liftIO $ bootstrapTA appContext talContent database

    result <- forM as wait
    putStrLn $ "done: " <> show result
    pure ()

runHttpApi :: IO ()
runHttpApi = Warp.run 9999 httpApi  

createAppContext :: IO AppContext
createAppContext = do 
    log'         <- createLogger
    dynamicState <- createDynamicState
    pure $ AppContext {        
        logger = log',
        config = Config {
            parallelism = getParallelism,
            rsyncConf = RsyncConf "/tmp/rsync",
            validationConfig = ValidationConfig $ 24 * 3600 
        },
        dynamicState = dynamicState
    }

createLogger :: IO AppLogger
createLogger = do 
    -- TODO Use colog-concurrent instead of this
    lock <- newMVar True
    pure $ AppLogger fullMessageAction lock
    where
        fullMessageAction = upgradeMessageAction defaultFieldMap $ 
            cmapM fmtRichMessageDefault logTextStdout  
          

