module Internal.CapstoneSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.Capstone

import Internal.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.Capstone" $ do
    csSkipdataStructStorableSpec
    csSetSkipdataSpec
    csDetailStorableSpec
    peekDetailSpec
    csInsnStorableSpec
    peekWithArchSpec
    csInsnOffsetSpec
    csDisasmSpec
    csDisasmIterSpec

csSkipdataStructStorableSpec :: Spec
csSkipdataStructStorableSpec = describe "Storable CsSkipdataStruct" $ do
    it "is a packed struct" $ 
        sizeOf (undefined :: CsSkipdataStruct) == 3 * sizeOf (0 :: WordPtr)
    it "has matching peek- and poke-implementations" $ property $
        \s@CsSkipdataStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

csSetSkipdataSpec :: Spec
csSetSkipdataSpec = describe "csSetSkipdata" $ do
    it "resets correctly" pending
    it "sets correctly" pending
    it "throws errors correctly" pending

csDetailStorableSpec :: Spec
csDetailStorableSpec = describe "Storable CsDetail" $ do
    it "has a memory layout we can manage" $
        sizeOf (undefined :: CsDetail) == 43 + 5 + 1480
    it "has matching peek- and poke-implementations with no arch specifics" $
        property $ \s@CsDetail{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

peekDetailSpec :: Spec
peekDetailSpec = describe "peekDetail" $ do
    it "returns results when a pointer is given" pending

csInsnStorableSpec :: Spec
csInsnStorableSpec = describe "Storable CsInsn" $ do
    it "has a memory layout we can manage" $
        sizeOf (undefined :: CsInsn) ==
            4 + 4 + 8 + 2 + 208 + sizeOf nullPtr +
                if sizeOf nullPtr == 4 then 2 else 6 -- pointer sizes
    it "has matching peek- and poke-implementations with no detail" $
        property $ \s@CsInsn{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

peekWithArchSpec :: Spec
peekWithArchSpec = describe "Storable CsInsn" $ do
    it "returns results when a pointer is given" pending

csInsnOffsetSpec :: Spec
csInsnOffsetSpec = describe "csInsnOffset" $ do
    it "behaves correctly (sums match)" pending

csDisasmSpec :: Spec
csDisasmSpec = describe "csDisasm" $ do
    it "disassembles a correct number of instructions" pending
    it "integrates with CsArch" pending

csDisasmIterSpec :: Spec
csDisasmIterSpec = describe "csDisasmIter" $ do
    it "fulfills modification guarantees" pending
    it "integrates with CsArch" pending
