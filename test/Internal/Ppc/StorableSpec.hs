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

getPpcOpMemStruct :: IO PpcOpMemStruct
getPpcOpMemStruct = do
    ptr <- mallocArray (sizeOf ppcOpMemStruct) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum PpcRegCc :: Int32)
    poke (plusPtr ptr 4) (24 :: Int32)
    peek (castPtr ptr) <* free ptr

ppcOpMemStruct :: PpcOpMemStruct
ppcOpMemStruct = PpcOpMemStruct PpcRegCc 24

-- | PpcOpMemStruct spec
ppcOpMemStructSpec :: Spec
ppcOpMemStructSpec = describe "Storable PpcOpMemStruct" $ do
    it "is a packed struct" $
        sizeOf (undefined :: PpcOpMemStruct) == 4 * 2
    it "has matching peek- and poke-implementations" $ property $
        \s@PpcOpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getPpcOpMemStruct `shouldReturn` ppcOpMemStruct

getPpcOpCrxStruct :: IO PpcOpCrxStruct
getPpcOpCrxStruct = do
    ptr <- mallocArray (sizeOf ppcOpCrxStruct) :: IO (Ptr Word8)
    poke (castPtr ptr) (0x01234567 :: Word32)
    poke (plusPtr ptr 4) (fromIntegral $ fromEnum PpcRegCc :: Int32)
    poke (plusPtr ptr 8) (fromIntegral $ fromEnum PpcBcGe :: Int32)
    peek (castPtr ptr) <* free ptr

ppcOpCrxStruct :: PpcOpCrxStruct
ppcOpCrxStruct = PpcOpCrxStruct 0x01234567 PpcRegCc PpcBcGe

-- | PpcOpCrxStruct spec
ppcOpCrxStructSpec :: Spec
ppcOpCrxStructSpec = describe "Storable PpcOpCrxStruct" $ do
    it "is a packed struct" $
        sizeOf (undefined :: PpcOpCrxStruct) == 4 * 3
    it "has matching peek- and poke-implementations" $ property $
        \s@PpcOpCrxStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getPpcOpCrxStruct `shouldReturn` ppcOpCrxStruct

getCsPpcOp :: IO CsPpcOp
getCsPpcOp = do
    ptr <- mallocArray (sizeOf csPpcOp) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum PpcOpImm :: Int32)
    poke (plusPtr ptr 4) (0x01234567 :: Int32)
    peek (castPtr ptr) <* free ptr

csPpcOp :: CsPpcOp
csPpcOp = Imm 0x01234567

-- | CsPpcipsOp spec
csPpcOpSpec :: Spec
csPpcOpSpec = describe "Storable CsPpcOp" $ do
    it "is a packed struct" $
        sizeOf (undefined :: CsPpcOp) == 4 + 12
    it "has matching peek- and poke-implementations" $ property $
        \s ->
            alloca (\p -> poke p s >> (peek p :: IO CsPpcOp)) `shouldReturn` s
    it "parses correctly" $ getCsPpcOp `shouldReturn` csPpcOp

getCsPpc :: IO CsPpc
getCsPpc = do
    ptr <- mallocArray (sizeOf csPpc) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum PpcBcLe :: Int32) 
    poke (plusPtr ptr 4) (fromIntegral $ fromEnum PpcBhPlus :: Int32) 
    poke (plusPtr ptr 8) (1 :: Word8)
    poke (plusPtr ptr 9) (1 :: Word8)
    poke (plusPtr ptr 12) csPpcOp
    peek (castPtr ptr) <* free ptr

csPpc :: CsPpc
csPpc = CsPpc PpcBcLe PpcBhPlus True [csPpcOp]

-- | CsPpc spec
csPpcSpec :: Spec
csPpcSpec = describe "Storable CsPpc" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsPpc) == 4 * 2 + 2 + 2 + 16 * 8
    it "has matching peek- and poke-implementations" $ property $
        \s@CsPpc{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getCsPpc `shouldReturn` csPpc
