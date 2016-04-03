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

getSparcOpMemStruct :: IO SparcOpMemStruct
getSparcOpMemStruct = do
    ptr <- mallocArray (sizeOf sparcOpMemStruct) :: IO (Ptr Word8)
    poke ptr 0x1
    poke (plusPtr ptr 1) (0x23 :: Word8)
    poke (plusPtr ptr 4) (0x01234567 :: Word32)
    peek (castPtr ptr) <* free ptr

sparcOpMemStruct :: SparcOpMemStruct
sparcOpMemStruct = SparcOpMemStruct 0x1 0x23 0x01234567

-- | SparcOpMemStruct spec
sparcOpMemStructSpec :: Spec
sparcOpMemStructSpec = describe "Storable SparcOpMemStruct" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: SparcOpMemStruct) == 2 + 2 + 4
    it "has matching peek- and poke-implementations" $ property $
        \s@SparcOpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getSparcOpMemStruct `shouldReturn` sparcOpMemStruct

getCsSparcOp :: IO CsSparcOp
getCsSparcOp = do
    ptr <- mallocArray (sizeOf csSparcOp) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum SparcOpImm :: Int32)
    poke (plusPtr ptr 4) (234 :: Int32)
    peek (castPtr ptr) <* free ptr

csSparcOp :: CsSparcOp
csSparcOp = Imm 234

-- | CsSparcipsOp spec
csSparcOpSpec :: Spec
csSparcOpSpec = describe "Storable CsSparcOp" $ do
    it "is a packed struct" $
        sizeOf (undefined :: CsSparcOp) == 4 + 8
    it "has matching peek- and poke-implementations" $ property $
        \s ->
            alloca (\p -> poke p s >> (peek p :: IO CsSparcOp)) `shouldReturn` s
    it "parses correctly" $ getCsSparcOp `shouldReturn` csSparcOp

getCsSparc :: IO CsSparc
getCsSparc = do
    ptr <- mallocArray (sizeOf csSparc) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum SparcCcIccL :: Int32)
    poke (plusPtr ptr 4) (fromIntegral $ fromEnum SparcHintA :: Int32)
    poke (plusPtr ptr 8) (1 :: Word8)
    poke (plusPtr ptr 12) csSparcOp
    peek (castPtr ptr) <* free ptr

csSparc :: CsSparc
csSparc = CsSparc SparcCcIccL SparcHintA [csSparcOp]

-- | CsSparc spec
csSparcSpec :: Spec
csSparcSpec = describe "Storable CsSparc" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsSparc) == 4 * 2 + 1 + 3 + 12 * 4
    it "has matching peek- and poke-implementations" $ property $
        \s@CsSparc{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getCsSparc `shouldReturn` csSparc
