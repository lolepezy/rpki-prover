module RPKI.Validation.ValidationWorkSpec where

import           Control.Lens hiding (children)

import qualified Data.ByteString.Short            as BSS
import qualified Data.Set                         as Set

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Resources.Types
import           RPKI.TestCommons
import           RPKI.Time
import           RPKI.Util                        (sha256s)
import           RPKI.Validation.Common           (createVerifiedResources)
import           RPKI.Validation.Fixtures
import           RPKI.Validation.ObjectValidation (validateCrl, validateRoa)


validationWorkGroup :: TestTree
validationWorkGroup =
    testGroup "Work of validating a CA"
        [ HU.testCase "Checks what doesn't need cryptography before signatures"
            shouldCheckBeforeSignatures
        , HU.testCase "Falls back from so many manifests at most"
            shouldLimitManifestFallbacks
        ]


-- | An object that is not valid for other reasons doesn't cost a signature
-- check, the signature is checked last. Here the signatures are not valid.
shouldCheckBeforeSignatures :: HU.Assertion
shouldCheckBeforeSignatures = do
    now   <- thisInstant
    key   <- newKey "ca"
    other <- newKey "other"
    let ca  = makeCaCert now other key 1 (ipv4s ["10.0.0.0/16"])
        crl = makeCrl now key & #signCrl . #revokedSerials .~ Set.singleton (Serial 11)
        forged :: WellStructuredRoa -> WellStructuredRoa
        forged roa = roa & #eeCert . #signature .~ sign other (BSS.fromShort roa.eeCert.encoded)
        validate roa = fst <$> runValidatorIO (newScopes "order")
                        (validateRoa StrictRFC now roa ca (Validated crl) 
                            (Just $ createVerifiedResources ca))

    validate (forged $ makeRoa now key 10 (ASN 1) "10.0.1.0/24") >>= \case
        Left (ValidationE (InvalidSignature _)) -> pure ()
        other_ -> HU.assertFailure $ "The signature is not valid: " <> show other_
    validate (forged $ makeRoa now key 12 (ASN 1) "10.1.0.0/24") >>= \case
        Left (ValidationE (OverclaimedResources _)) -> pure ()
        other_ -> HU.assertFailure $ "Resources go before the signature: " <> show other_
    validate (forged $ makeRoa now key 11 (ASN 1) "10.0.1.0/24") >>= 
        HU.assertEqual "Revocation goes before the signature" 
            (Left $ ValidationE RevokedResourceCertificate)

    let expired = makeCrl now key 
                    & #signCrl . #nextUpdateTime .~ momentAfter (unNow now) (-60)
                    & #signCrl . #signatureValue .~ sign other "CRL ca"
    fst <$> runValidatorIO (newScopes "crl") (validateCrl now expired ca) >>= \case
        Left (ValidationE NextUpdateTimeIsInThePast {}) -> pure ()
        other_ -> HU.assertFailure $ "The times go before the signature: " <> show other_


-- | The key of a CA can sign any number of manifests that are not valid, 
-- and every one of them that is tried validates its children
shouldLimitManifestFallbacks :: HU.Assertion
shouldLimitManifestFallbacks =
    withTestContext $ \appContext -> do
        now    <- thisInstant
        issuer <- newKey "issuer"
        key    <- newKey "key"
        let ca     = makeCaCert now issuer key 1 (ipv4s ["10.0.0.0/16"])
            crl    = makeCrl now key
            broken n = makeManifest now key n 
                        [("ca.crl", crl.hash), ("missing.roa", sha256s "not in the cache")]
        storeCertificates appContext now [ca]
        storeObjects appContext now $ CrlRO crl : [ MftRO (broken n) | n <- [1 .. 15] ]

        Run {..} <- validateInOrder appContext now [ca]
        let maxFallbacks = appContext ^. #config . #validationConfig . #maxManifestFallbacks
        HU.assertEqual "Falls back as many times as it's allowed" 
            maxFallbacks (length [ () | MftFallback {} <- issues state ])
        HU.assertEqual "Tells about it" 
            [TooManyManifestFallbacks maxFallbacks] [ e | e@TooManyManifestFallbacks {} <- issues state ]
