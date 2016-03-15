module Internal.SystemZ.StorableSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.SystemZ

import Internal.SystemZ.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.SysZ" $ do
    syszOpMemStructSpec
    csSysZOpSpec
    csSysZSpec

-- | SysZOpMemStruct spec
syszOpMemStructSpec :: Spec
syszOpMemStructSpec = describe "Storable SysZOpMemStruct" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: SysZOpMemStruct) == 2 + 6 + 8 * 2
    it "has matching peek- and poke-implementations" $ property $
        \s@SysZOpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsSysZipsOp spec
csSysZOpSpec :: Spec
csSysZOpSpec = describe "Storable CsSysZOp" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: CsSysZOp) == 4 + 4 + 24
    it "has matching peek- and poke-implementations" $ property $
        \s ->
            alloca (\p -> poke p s >> (peek p :: IO CsSysZOp)) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsSysZ spec
csSysZSpec :: Spec
csSysZSpec = describe "Storable CsSysZ" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsSysZ) == 4 + 1 + 3 + 32 * 6
    it "has matching peek- and poke-implementations" $ property $
        \s@CsSysZ{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"
