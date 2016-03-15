module Internal.X86.StorableSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.X86

import Internal.X86.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.X86" $ do
    x86OpMemStructSpec
    csX86OpSpec
    csX86Spec

-- | X86OpMemStruct spec
x86OpMemStructSpec :: Spec
x86OpMemStructSpec = describe "Storable X86OpMemStruct" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: X86OpMemStruct) == 4 * 4 + 8
    it "has matching peek- and poke-implementations" $ property $
        \s@X86OpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsX86ipsOp spec
csX86OpSpec :: Spec
csX86OpSpec = describe "Storable CsX86Op" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: CsX86Op) == 4 + 4 + 24 + 1 + 3 + 4 + 1 + 7
    it "has matching peek- and poke-implementations" $ property $
        \s ->
            alloca (\p -> poke p s >> (peek p :: IO CsX86Op)) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsX86 spec
csX86Spec :: Spec
csX86Spec = describe "Storable CsX86" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsX86) ==
            4 * 5 + 1 + 3 + 4 * 3 + 1 + 3 + 4 + 1 + 3 + 48 * 8
    it "has matching peek- and poke-implementations" $ property $
        \s@CsX86{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"
