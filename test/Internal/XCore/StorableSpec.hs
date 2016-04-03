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

getXCoreOpMemStruct :: IO XCoreOpMemStruct
getXCoreOpMemStruct = do
    ptr <- mallocArray (sizeOf xCoreOpMemStruct) :: IO (Ptr Word8)
    poke ptr 0x1
    poke (plusPtr ptr 1) (0x2 :: Word8)
    poke (plusPtr ptr 4) (0x01234567 :: Int32)
    poke (plusPtr ptr 8) (1 :: Int32)
    peek (castPtr ptr) <* free ptr

xCoreOpMemStruct :: XCoreOpMemStruct
xCoreOpMemStruct = XCoreOpMemStruct 0x1 0x2 0x01234567 1

-- | XCoreOpMemStruct spec
xCoreOpMemStructSpec :: Spec
xCoreOpMemStructSpec = describe "Storable XCoreOpMemStruct" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: XCoreOpMemStruct) == 2 + 2 + 4*2
    it "has matching peek- and poke-implementations" $ property $
        \s@XCoreOpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getXCoreOpMemStruct `shouldReturn` xCoreOpMemStruct

getCsXCoreOp :: IO CsXCoreOp
getCsXCoreOp = do
    ptr <- mallocArray (sizeOf csXCoreOp) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum XcoreOpImm :: Int32)
    poke (plusPtr ptr 4) (0x01234567 :: Int32)
    peek (castPtr ptr) <* free ptr

csXCoreOp :: CsXCoreOp
csXCoreOp = Imm 0x01234567

-- | CsXCoreipsOp spec
csXCoreOpSpec :: Spec
csXCoreOpSpec = describe "Storable CsXCoreOp" $ do
    it "is a packed struct" $
        sizeOf (undefined :: CsXCoreOp) == 4 + 12
    it "has matching peek- and poke-implementations" $ property $
        \s -> alloca (\p -> poke p s >> (peek p :: IO CsXCoreOp))
        `shouldReturn` s
    it "parses correctly" $ getCsXCoreOp `shouldReturn` csXCoreOp

getCsXCore :: IO CsXCore
getCsXCore = do
    ptr <- mallocArray (sizeOf csXCore) :: IO (Ptr Word8)
    poke ptr 1
    poke (plusPtr ptr 4) csXCoreOp
    peek (castPtr ptr) <* free ptr

csXCore :: CsXCore
csXCore = CsXCore [csXCoreOp]

-- | CsXCore spec
csXCoreSpec :: Spec
csXCoreSpec = describe "Storable CsXCore" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsXCore) == 1 + 3 + 16 * 8
    it "has matching peek- and poke-implementations" $ property $
        \s@CsXCore{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getCsXCore `shouldReturn` csXCore
