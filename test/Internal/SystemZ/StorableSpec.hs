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

getSyszOpMemStruct :: IO SysZOpMemStruct
getSyszOpMemStruct = do
    ptr <- mallocArray (sizeOf syszOpMemStruct) :: IO (Ptr Word8)
    poke (castPtr ptr) (0x1 :: Word8)
    poke (plusPtr ptr 1) (0x2 :: Word8)
    poke (plusPtr ptr 8) (0x0123456789abcdef :: Word64)
    poke (plusPtr ptr 16) (0x1032547698badcfe :: Word64)
    peek (castPtr ptr) <* free ptr

syszOpMemStruct :: SysZOpMemStruct
syszOpMemStruct = SysZOpMemStruct 0x1 0x2 0x0123456789abcdef 0x1032547698badcfe

-- | SysZOpMemStruct spec
syszOpMemStructSpec :: Spec
syszOpMemStructSpec = describe "Storable SysZOpMemStruct" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: SysZOpMemStruct) == 2 + 6 + 8 * 2
    it "has matching peek- and poke-implementations" $ property $
        \s@SysZOpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getSyszOpMemStruct `shouldReturn` syszOpMemStruct

getCsSyszOp :: IO CsSysZOp
getCsSyszOp = do
    ptr <- mallocArray (sizeOf csSyszOp) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum SyszOpImm :: Int32)
    poke (plusPtr ptr 8) (19237 :: Int64)
    peek (castPtr ptr) <* free ptr

csSyszOp :: CsSysZOp
csSyszOp = Imm 19237 

-- | CsSysZipsOp spec
csSysZOpSpec :: Spec
csSysZOpSpec = describe "Storable CsSysZOp" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: CsSysZOp) == 4 + 4 + 24
    it "has matching peek- and poke-implementations" $ property $
        \s ->
            alloca (\p -> poke p s >> (peek p :: IO CsSysZOp)) `shouldReturn` s
    it "parses correctly" $ getCsSyszOp `shouldReturn` csSyszOp

getCsSysz :: IO CsSysZ
getCsSysz = do
    ptr <- mallocArray (sizeOf csSysz) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum SyszCcL :: Int32)
    poke (plusPtr ptr 4) (1 :: Word8)
    poke (plusPtr ptr 8) csSyszOp
    peek (castPtr ptr) <* free ptr

csSysz :: CsSysZ
csSysz = CsSysZ SyszCcL [csSyszOp]

-- | CsSysZ spec
csSysZSpec :: Spec
csSysZSpec = describe "Storable CsSysZ" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsSysZ) == 4 + 1 + 3 + 32 * 6
    it "has matching peek- and poke-implementations" $ property $
        \s@CsSysZ{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getCsSysz `shouldReturn` csSysz
