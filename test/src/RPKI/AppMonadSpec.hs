{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module RPKI.AppMonadSpec where

import           Control.Monad           (when, forM, unless)
import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set
import qualified Data.Text               as Text

import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck   as QC

import qualified Test.Tasty.HUnit        as HU

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Orphans
import           RPKI.Parallel
import           RPKI.Reporting
import           RPKI.Repository
import RPKI.Util (toNatural)
import Data.Maybe (fromMaybe)


isSemigroup :: Eq s => Semigroup s => (s, s, s) -> Bool
isSemigroup (s1, s2, s3) = s1 <> (s2 <> s3) == (s1 <> s2) <> s3


runValidatorTAndvalidatorTShouldBeId :: QC.Property
runValidatorTAndvalidatorTShouldBeId = monadicIO $ do
  z :: (Either AppError (), ValidationState) <- pick arbitrary 
  q <- runValidatorT (newValidatorPath "zzz") $ validatorT $ pure z
  assert $ q == z

forMShouldSavesState :: HU.Assertion
forMShouldSavesState = do
  v <- QC.generate arbitrary  
     
  (_, ValidationState { validations = Validations validationMap, .. }) 
    <- runValidatorT (newValidatorPath "zzz") $ do 
        validatorT $ pure (Right (), v)
        forM ["x", "y", "z"] $ \x ->
            appWarn $ UnspecifiedE x (x <> "-bla") 
  
  HU.assertEqual "Not the same Validations" 
    (Map.lookup (newPath "zzz") validationMap)
     (Just $ Set.fromList [
         VWarn (VWarning (UnspecifiedE "x" "x-bla")),
         VWarn (VWarning (UnspecifiedE "y" "y-bla")),
         VWarn (VWarning (UnspecifiedE "z" "z-bla"))])


appMonadSpec :: TestTree
appMonadSpec = testGroup "AppMonad" [
        QC.testProperty "ValidationState is a semigroup" (isSemigroup @ValidationState),
        QC.testProperty "RrdpSource is a semigroup" (isSemigroup @RrdpSource),

        QC.testProperty
            "runValidatorT . validatorT == id"
            runValidatorTAndvalidatorTShouldBeId,
            
        HU.testCase
            "forM saves state"
            forMShouldSavesState
    ]
