module Internal.Ppc.StorableSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.Ppc

import Internal.Ppc.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.Ppc" $ do
    ppcOpMemStructSpec
    ppcOpCrxStructSpec
    csPpcOpSpec
    csPpcSpec

-- | PpcOpMemStruct spec
ppcOpMemStructSpec :: Spec
ppcOpMemStructSpec = describe "Storable PpcOpMemStruct" $ do
    it "is a packed struct" $
        sizeOf (undefined :: PpcOpMemStruct) == 4 * 2
    it "has matching peek- and poke-implementations" $ property $
        \s@PpcOpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | PpcOpCrxStruct spec
ppcOpCrxStructSpec :: Spec
ppcOpCrxStructSpec = describe "Storable PpcOpCrxStruct" $ do
    it "is a packed struct" $
        sizeOf (undefined :: PpcOpCrxStruct) == 4 * 3
    it "has matching peek- and poke-implementations" $ property $
        \s@PpcOpCrxStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsPpcipsOp spec
csPpcOpSpec :: Spec
csPpcOpSpec = describe "Storable CsPpcOp" $ do
    it "is a packed struct" $
        sizeOf (undefined :: CsPpcOp) == 4 + 12
    it "has matching peek- and poke-implementations" $ property $
        \s ->
            alloca (\p -> poke p s >> (peek p :: IO CsPpcOp)) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsPpc spec
csPpcSpec :: Spec
csPpcSpec = describe "Storable CsPpc" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsPpc) == 4 * 2 + 2 + 2 + 16 * 8
    it "has matching peek- and poke-implementations" $ property $
        \s@CsPpc{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"
