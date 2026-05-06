module RPKI.Store.SerialisationSpec where

import           Data.Int                  (Int64)
import qualified Data.ByteString           as BS
import           Test.Tasty
import qualified Test.Tasty.QuickCheck     as QC

import           RPKI.Orphans
import           RPKI.Store.Base.Serialisation
import           RPKI.Domain (ArtificialKey(..))

serialisationSpec :: TestTree
serialisationSpec = testGroup "Serialisation ordering properties"
    [ testGroup "LexOrdKey64"
        [ QC.testProperty "order preserved: a < b => encode(a) < encode(b)" $
            \a b -> compare (LexOrdKey64 a) (LexOrdKey64 b)
                        == compare (serialise_ (LexOrdKey64 a)) (serialise_ (LexOrdKey64 b))

        , QC.testProperty "round-trip: decode . encode == id" $
            \a -> deserialise_ (serialise_ (LexOrdKey64 a)) == LexOrdKey64 (a :: Int64)

        , QC.testProperty "serialised form is always 8 bytes" $
            \a -> BS.length (serialise_ (LexOrdKey64 (a :: Int64))) == 8

        , QC.testProperty "minBound serialises to all-zeros" $
            serialise_ (LexOrdKey64 minBound) == BS.replicate 8 0x00

        , QC.testProperty "maxBound serialises to all-0xFF" $
            serialise_ (LexOrdKey64 maxBound) == BS.replicate 8 0xFF

        , QC.testProperty "0 serialises with high bit set, rest zero" $
            serialise_ (LexOrdKey64 0) == BS.pack [0x80, 0, 0, 0, 0, 0, 0, 0]

        , QC.testProperty "(-1) < 0 in serialised order" $
            serialise_ (LexOrdKey64 (-1)) < serialise_ (LexOrdKey64 0)
        ]

    , testGroup "ArtificialKey"
        [ QC.testProperty "serialised form is always 8 bytes" $
            \a -> BS.length (serialise_ (ArtificialKey (a :: LexOrdKey64))) == 8
        ]
    ]
