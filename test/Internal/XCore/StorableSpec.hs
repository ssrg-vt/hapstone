module Internal.XCore.StorableSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.XCore

import Internal.XCore.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.XCore" $ do
    xCoreOpMemStructSpec
    csXCoreOpSpec
    csXCoreSpec

-- | XCoreOpMemStruct spec
xCoreOpMemStructSpec :: Spec
xCoreOpMemStructSpec = describe "Storable XCoreOpMemStruct" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: XCoreOpMemStruct) == 2 + 2 + 4*2
    it "has matching peek- and poke-implementations" $ property $
        \s@XCoreOpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsXCoreipsOp spec
csXCoreOpSpec :: Spec
csXCoreOpSpec = describe "Storable CsXCoreOp" $ do
    it "is a packed struct" $
        sizeOf (undefined :: CsXCoreOp) == 4 + 12
    it "has matching peek- and poke-implementations" $ property $
        \s -> alloca (\p -> poke p s >> (peek p :: IO CsXCoreOp))
        `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsXCore spec
csXCoreSpec :: Spec
csXCoreSpec = describe "Storable CsXCore" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsXCore) == 1 + 3 + 16 * 8
    it "has matching peek- and poke-implementations" $ property $
        \s@CsXCore{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"
