{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module RPKI.AppMonadSpec where

import           Control.Monad
import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set
import           Data.Proxy

import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck   as QC

import qualified Test.Tasty.HUnit        as HU

import           RPKI.AppMonad
import           RPKI.Orphans
import           RPKI.Reporting

isSemigroup :: Eq s => Semigroup s => (s, s, s) -> Bool
isSemigroup (s1, s2, s3) = s1 <> (s2 <> s3) == (s1 <> s2) <> s3


runValidatorTAndvalidatorTShouldBeId :: QC.Property
runValidatorTAndvalidatorTShouldBeId = monadicIO $ do
  z :: (Either AppError (), ValidationState) <- pick arbitrary 
  q <- runValidatorT (newScopes "zzz") $ validatorT $ pure z
  assert $ q == z

forMShouldSavesState :: HU.Assertion
forMShouldSavesState = do
  v <- QC.generate arbitrary  
     
  (_, ValidationState { validations = Validations validationMap }) 
    <- runValidatorT (newScopes "zzz") $ do 
        validatorT $ pure (Right (), v)
        forM ["x", "y", "z"] $ \x ->
            appWarn $ UnspecifiedE x (x <> "-bla") 
  
  HU.assertEqual "Not the same Validations" 
    (Map.lookup (newScope "zzz") validationMap)
     (Just $ Set.fromList [
         VWarn (VWarning (UnspecifiedE "x" "x-bla")),
         VWarn (VWarning (UnspecifiedE "y" "y-bla")),
         VWarn (VWarning (UnspecifiedE "z" "z-bla"))])

scopesShouldBeProperlyNested :: HU.Assertion
scopesShouldBeProperlyNested = do
    (_, ValidationState { validations = Validations validationMap }) 
        <- runValidatorT (newScopes "root") $ do
            timedMetric (Proxy :: Proxy RrdpMetric) $ do                 
                appWarn $ UnspecifiedE "Error0" "text 0"
                vFocusOn TextFocus "snapshot.xml" $ do            
                    timedMetric (Proxy :: Proxy RsyncMetric) $ do                        
                        appWarn $ UnspecifiedE "Error1" "text 1"
                        vFocusOn TextFocus "broken.roa" $ do                                        
                            appError $ UnspecifiedE "Crash" "Crash it"                                                                    

    HU.assertEqual "Deepest scope should have 1 error"     
        (Map.lookup (subScope' TextFocus "broken.roa" 
                        $ subScope' TextFocus "snapshot.xml" 
                        $ newScope "root") validationMap)
        (Just $ Set.fromList [VErr (UnspecifiedE "Crash" "Crash it")])

    HU.assertEqual "Nested scope should have 1 warning"         
        (Map.lookup (subScope' TextFocus "snapshot.xml" $ newScope "root") validationMap)
        (Just $ Set.fromList [VWarn (VWarning (UnspecifiedE "Error1" "text 1"))])

    HU.assertEqual "Nested validations should have 1 warning"         
        (Map.lookup (newScope "root") validationMap)
        (Just $ Set.fromList [VWarn (VWarning (UnspecifiedE "Error0" "text 0"))])



appMonadSpec :: TestTree
appMonadSpec = testGroup "AppMonad" [
        QC.testProperty "ValidationState is a semigroup" (isSemigroup @ValidationState),
        QC.testProperty "RrdpSource is a semigroup" (isSemigroup @RrdpSource),

        QC.testProperty "runValidatorT . validatorT == id" runValidatorTAndvalidatorTShouldBeId,
            
        HU.testCase "forM saves state" forMShouldSavesState,
        HU.testCase "forM saves state" scopesShouldBeProperlyNested
    ]
