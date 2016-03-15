module Internal.Sparc.StorableSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.Sparc

import Internal.Sparc.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.Sparc" $ do
    sparcOpMemStructSpec
    csSparcOpSpec
    csSparcSpec

-- | SparcOpMemStruct spec
sparcOpMemStructSpec :: Spec
sparcOpMemStructSpec = describe "Storable SparcOpMemStruct" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: SparcOpMemStruct) == 2 + 2 + 4
    it "has matching peek- and poke-implementations" $ property $
        \s@SparcOpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsSparcipsOp spec
csSparcOpSpec :: Spec
csSparcOpSpec = describe "Storable CsSparcOp" $ do
    it "is a packed struct" $
        sizeOf (undefined :: CsSparcOp) == 4 + 8
    it "has matching peek- and poke-implementations" $ property $
        \s ->
            alloca (\p -> poke p s >> (peek p :: IO CsSparcOp)) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsSparc spec
csSparcSpec :: Spec
csSparcSpec = describe "Storable CsSparc" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsSparc) == 4 * 2 + 1 + 3 + 12 * 4
    it "has matching peek- and poke-implementations" $ property $
        \s@CsSparc{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"
