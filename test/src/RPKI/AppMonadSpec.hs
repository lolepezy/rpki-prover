module RPKI.AppMonadSpec where

import           Control.Monad
import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set
import           Data.Proxy

import           Test.Tasty
import           Test.Tasty.QuickCheck   as QC

import qualified Test.Tasty.HUnit        as HU

import           RPKI.AppMonad
import           RPKI.Orphans
import           RPKI.Reporting

isSemigroup :: Eq s => Semigroup s => (s, s, s) -> Bool
isSemigroup (s1, s2, s3) = s1 <> (s2 <> s3) == (s1 <> s2) <> s3


forMShouldSavesState :: HU.Assertion
forMShouldSavesState = do
  v <- QC.generate arbitrary  
     
  (_, ValidationState { validations = Validations validationMap }) 
    <- runValidatorIO (newScopes "zzz") $ do 
        putVState v
        forM ["x", "y", "z"] $ \x ->
            appWarn $ InternalE (InternalError (x <> "-bla")) 
  
  HU.assertEqual "Not the same Validations" 
    (Map.lookup (newScope "zzz") validationMap)
     (Just $ Set.fromList [
         VWarn (VWarning (InternalE (InternalError "x-bla"))),
         VWarn (VWarning (InternalE (InternalError "y-bla"))),
         VWarn (VWarning (InternalE (InternalError "z-bla")))])

scopesShouldBeProperlyNested :: HU.Assertion
scopesShouldBeProperlyNested = do
    (_, ValidationState { validations = Validations validationMap }) 
        <- runValidatorIO (newScopes "root") $ do
            timedMetric (Proxy :: Proxy RrdpMetric) $ do                 
                appWarn $ InternalE (InternalError "Error0: text 0")
                vFocusOn TextFocus "snapshot.xml" $ do            
                    timedMetric (Proxy :: Proxy TraverseMetric) $ do                        
                        appWarn $ InternalE (InternalError "Error1: text 1")
                        vFocusOn TextFocus "broken.roa" $ do                                        
                            appError $ InternalE (InternalError "Crash: Crash it")                                                                    

    HU.assertEqual "Deepest scope should have 1 error"     
        (Map.lookup (subScope TextFocus "broken.roa" 
                        $ subScope TextFocus "snapshot.xml" 
                        $ newScope "root") validationMap)
        (Just $ Set.fromList [VErr (InternalE (InternalError "Crash: Crash it"))])

    HU.assertEqual "Nested scope should have 1 warning"         
        (Map.lookup (subScope TextFocus "snapshot.xml" $ newScope "root") validationMap)
        (Just $ Set.fromList [VWarn (VWarning (InternalE (InternalError "Error1: text 1")))])

    HU.assertEqual "Nested validations should have 1 warning"         
        (Map.lookup (newScope "root") validationMap)
        (Just $ Set.fromList [VWarn (VWarning (InternalE (InternalError "Error0: text 0")))])



appMonadSpec :: TestTree
appMonadSpec = testGroup "AppMonad" [
        QC.testProperty "ValidationState is a semigroup" (isSemigroup @ValidationState),
        QC.testProperty "RrdpSource is a semigroup" (isSemigroup @RrdpSource),
        QC.testProperty "HttpStatus is a semigroup" (isSemigroup @HttpStatus),

        HU.testCase "forM saves state" forMShouldSavesState,
        HU.testCase "scopes are properly nested" scopesShouldBeProperlyNested
    ]
