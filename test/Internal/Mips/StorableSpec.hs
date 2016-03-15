module Internal.Mips.StorableSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.Mips

import Internal.Mips.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.Mips" $ do
    mipsOpMemStructSpec
    csMipsOpSpec
    csMipsSpec

-- | MipsOpMemStruct spec
mipsOpMemStructSpec :: Spec
mipsOpMemStructSpec = describe "Storable MipsOpMemStruct" $ do
    it "is a packed struct" $
        sizeOf (undefined :: MipsOpMemStruct) ==
            sizeOf (0 :: CUInt) * 2 + sizeOf (0 :: Word32) * 2
    it "has matching peek- and poke-implementations" $ property $
        \s@MipsOpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsMipsOp spec
csMipsOpSpec :: Spec
csMipsOpSpec = describe "Storable CsMipsOp" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsMipsOp) == 4 + 4 + 16
    it "has matching peek- and poke-implementations" $ property $
        \s ->
            alloca (\p -> poke p s >> (peek p :: IO CsMipsOp)) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsMips spec
csMipsSpec :: Spec
csMipsSpec = describe "Storable CsMips" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsMips) ==
            1 + 7 + 8 * sizeOf (undefined :: CsMipsOp)
    it "has matching peek- and poke-implementations" $ property $
        \s@CsMips{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"
