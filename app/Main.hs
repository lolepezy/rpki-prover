module Main where

import           Colog

import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted

import           Data.Bifunctor
import qualified Data.ByteString                 as BS

import qualified Network.Wai.Handler.Warp        as Warp

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Execution
import           RPKI.Http.Server
import           RPKI.Logging
import           RPKI.Store.Database
import           RPKI.Store.Util
import           RPKI.TAL
import           RPKI.TopDown
import           RPKI.Util                       (convert, fmtEx)
import           RPKI.Version

--
servantAPI :: IO ()
servantAPI = pure ()

main :: IO ()
main = do
    -- load config file and apply command line options
    -- initialise environment and LMDB

    lmdbEnv  <- mkLmdb "./data" 1000
    database' <- createDatabase lmdbEnv
    appContext <- createAppContext database'

    void $ concurrently
        (runHttpApi appContext)
        (runValidatorApp appContext)


-- runValidatorApp :: IO ()
runValidatorApp appContext = do     
    let tals = [ "afrinic.tal", "apnic.tal", "arin.tal", "lacnic.tal", "ripe.tal" ]
    -- let tals = [ "ripe.tal" ]

    void $ updateWorldVerion $ dynamicState appContext

    as <- forM tals $ \tal -> async $
        runValidatorT (vContext $ URI $ convert tal) $ do
            t <- fromTry (RsyncE . FileReadError . fmtEx) $             
                BS.readFile $ "/Users/mpuzanov/.rpki-cache/tals/" <> tal
            talContent <- vHoist $ fromEither $ first TAL_E $ parseTAL $ convert t                        
            liftIO $ bootstrapTA appContext talContent

    result <- forM as wait
    putStrLn $ "done: " <> show result
    pure ()

-- runHttpApi :: IO ()
runHttpApi appContext = Warp.run 9999 $ httpApi appContext

createAppContext :: DB s -> IO (AppContext s)
createAppContext database' = do 
    log'  <- createLogger
    state <- createDynamicState
    pure $ AppContext {        
        logger = log',
        config = Config {
            parallelism = getParallelism,
            rsyncConf = RsyncConf "/tmp/rsync",
            validationConfig = ValidationConfig $ 24 * 3600 
        },
        dynamicState = state,
        database = database'
    }

createLogger :: IO AppLogger
createLogger = do 
    -- TODO Use colog-concurrent instead of this
    lock <- newMVar True
    pure $ AppLogger fullMessageAction lock
    where
        fullMessageAction = upgradeMessageAction defaultFieldMap $ 
            cmapM fmtRichMessageDefault logTextStdout  
          

