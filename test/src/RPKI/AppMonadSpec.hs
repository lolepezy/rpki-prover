{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module RPKI.AppMonadSpec where

import           Control.Monad           (forM, unless)
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


concurrentTasksPreservesState :: QC.Property
concurrentTasksPreservesState = monadicIO $ do
  z1 :: (Either AppError (), ValidationState) <- pick arbitrary 
  z2 :: (Either AppError (), ValidationState) <- pick arbitrary 
  q <- run $ runValidatorT (newValidatorPath "zzz") $ 
                concurrentTasks 
                    (validatorT $ pure z1)
                    (validatorT $ pure z2)
  assert $ snd q == snd z1 <> snd z2
  assert $ fst q == ((,) <$> fst z1 <*> fst z2)


parallelTasksPreservesState :: HU.Assertion
parallelTasksPreservesState = do    
  b <- newBottleneckIO 2  

  let zs = take 20 $ map (Text.pack . show) [1..]  
  (_, ValidationState { validations = Validations validationMap, .. })     
        <- runValidatorT (newValidatorPath "zzz") $ do
                validatorT $ pure (Right (), mempty)
                parallelTasks b zs $ \z -> 
                    appWarn $ UnspecifiedE z (z <> "-bla") 
    
  HU.assertEqual "Not the same Validations"
        (Map.lookup (newPath "zzz") validationMap)
        (Just $ Set.fromList $ map (\z -> VWarn $ VWarning $ UnspecifiedE z (z <> "-bla")) zs)


appMonadSpec :: TestTree
appMonadSpec = testGroup "AppMonad" [
        QC.testProperty 
            "ValidationState is a semigroup" 
            (isSemigroup @ValidationState),

        QC.testProperty
            "runValidatorT . validatorT == id"
            runValidatorTAndvalidatorTShouldBeId,

        QC.testProperty
            "concurrentTasks keeps the state and validity"
            concurrentTasksPreservesState,

        HU.testCase
            "parallelTasks keeps the state and validity"
            parallelTasksPreservesState,
            
        HU.testCase
            "forM saves state"
            forMShouldSavesState
    ]
