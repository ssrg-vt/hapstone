module Internal.Mips.StorableSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.Mips

import Internal.Mips.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.Mips" $ do
    mipsOpMemStructSpec
    csMipsOpSpec
    csMipsSpec

getMipsOpMemStruct :: IO MipsOpMemStruct
getMipsOpMemStruct = do
    ptr <- mallocArray (sizeOf mipsOpMemStruct) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum MipsReg15 :: Word32)
    poke (plusPtr ptr 8) (0x0123456789abcdef :: Word64)
    peek (castPtr ptr) <* free ptr

mipsOpMemStruct :: MipsOpMemStruct
mipsOpMemStruct = MipsOpMemStruct MipsReg15 0x0123456789abcdef

-- | MipsOpMemStruct spec
mipsOpMemStructSpec :: Spec
mipsOpMemStructSpec = describe "Storable MipsOpMemStruct" $ do
    it "is a packed struct" $
        sizeOf (undefined :: MipsOpMemStruct) ==
            sizeOf (0 :: CUInt) * 2 + sizeOf (0 :: Word64)
    it "has matching peek- and poke-implementations" $ property $
        \s@MipsOpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getMipsOpMemStruct `shouldReturn` mipsOpMemStruct

getCsMipsOp :: IO CsMipsOp
getCsMipsOp = do
    ptr <- mallocArray (sizeOf csMipsOp) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum MipsOpImm :: Int32)
    poke (plusPtr ptr 8) (72324 :: Int64)
    peek (castPtr ptr) <* free ptr

csMipsOp :: CsMipsOp
csMipsOp = Imm 72324

-- | CsMipsOp spec
csMipsOpSpec :: Spec
csMipsOpSpec = describe "Storable CsMipsOp" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsMipsOp) == 4 + 4 + 16
    it "has matching peek- and poke-implementations" $ property $
        \s ->
            alloca (\p -> poke p s >> (peek p :: IO CsMipsOp)) `shouldReturn` s
    it "parses correctly" $ getCsMipsOp `shouldReturn` csMipsOp

getCsMips :: IO CsMips
getCsMips = do
    ptr <- mallocArray (sizeOf csMips) :: IO (Ptr Word8)
    poke (castPtr ptr) (1 :: Word8)
    poke (plusPtr ptr 8) csMipsOp
    peek (castPtr ptr) <* free ptr

csMips :: CsMips
csMips = CsMips [csMipsOp]

-- | CsMips spec
csMipsSpec :: Spec
csMipsSpec = describe "Storable CsMips" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsMips) ==
            1 + 7 + 8 * sizeOf (undefined :: CsMipsOp)
    it "has matching peek- and poke-implementations" $ property $
        \s@CsMips{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getCsMips `shouldReturn` csMips
