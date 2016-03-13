module Arm64Spec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.Arm64

import Arm64Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.Arm64" $ do
    arm64OpMemStructSpec
    csArm64OpSpec
    csArm64Spec

-- | Arm64OpMemStruct spec
arm64OpMemStructSpec :: Spec
arm64OpMemStructSpec = describe "Storable Arm64OpMemStruct" $ do
    it "is a packed struct" $
        sizeOf (undefined :: Arm64OpMemStruct) ==
            sizeOf (0 :: CUInt) * 2 + sizeOf (0 :: Word32)
    it "has matching peek- and poke-implementations" $ property $
        \s@(Arm64OpMemStruct _ _ _) ->
            (alloca $ \p -> (poke p s >> peek p)) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

csArm64OpSpec :: Spec
csArm64OpSpec = describe "Storable CsArm64Op" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsArm64Op) == 7*4 + 4 + 12 + 4
    it "has matching peek- and poke-implementations" $ property $
        \s@CsArm64Op{} ->
            (alloca $ \p -> (poke p s >> peek p)) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

csArm64Spec :: Spec
csArm64Spec = describe "Storable CsArm64" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsArm64) == 4 + 3*1 + 1 + 8*48
    it "has matching peek- and poke-implementations" $ property $
        \s@CsArm64{} ->
            (alloca $ \p -> (poke p s >> peek p)) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"
