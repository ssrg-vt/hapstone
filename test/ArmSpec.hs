module ArmSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.Arm

import ArmDefault

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.Arm" $ do
    armOpMemStructSpec
    csArmOpSpec
    csArmSpec

-- | ArmOpMemStruct spec
armOpMemStructSpec :: Spec
armOpMemStructSpec = describe "Storable ArmOpMemStruct" $ do
    it "is a packed struct" $
        sizeOf (undefined :: ArmOpMemStruct) ==
            sizeOf (0 :: CUInt) * 2 + sizeOf (0 :: Word32) * 2
    it "has matching peek- and poke-implementations" $ property $
        \s@ArmOpMemStruct{} ->
            (alloca $ \p -> (poke p s >> peek p)) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsArmOp spec
csArmOpSpec :: Spec
csArmOpSpec = describe "Storable CsArmOp" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsArmOp) == 4*4 + 16 + 1 + 7
    it "has matching peek- and poke-implementations" $ property $
        \s@CsArmOp{} ->
            (alloca $ \p -> (poke p s >> peek p)) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"

-- | CsArm spec
csArmSpec :: Spec
csArmSpec = describe "Storable CsArm" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsArm) ==
            1 + 3 + 5*4 + 2*1 + 2 + 4 + 1 + 7 + 36*40
    it "has matching peek- and poke-implementations" $ property $
        \s@CsArm{} ->
            (alloca $ \p -> (poke p s >> peek p)) `shouldReturn` s
    it "parses correctly" $ pendingWith "use a binary string generated"
