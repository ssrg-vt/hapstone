{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.M68K
Description : M68K architecture header ported using C2HS + some boilerplate
Copyright   : (c) Inokentiy Babushkin, 2016
License     : BSD3
Maintainer  : Inokentiy Babushkin <inokentiy.babushkin@googlemail.com>
Stability   : experimental

This module contains M68K specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.M68K where

#include <capstone/m68k.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

-- | M68K registers
{#enum m68k_reg as M68KReg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | M68K addressing modes
{#enum m68k_address_mode as M68KAddressMode {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | M68K operand type
{#enum m68k_op_type as M68KOpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | memory access operands
-- associated with 'M68KOpMem' operand type
data M68KOpMemStruct = M68KOpMemStruct 
    { baseReg :: M68KReg -- ^ base register
    , indexReg :: M68KReg -- ^ index register
    , inBaseReg :: M68KReg -- ^ indirect base register
    , inDisp :: Word32 -- ^ indirect displacement
    , outDisp :: Word32 -- ^ other displacement
    , disp :: Word16 -- ^ displacement value
    , scale :: Word8 -- ^ scale for index register
    -- TODO: use ADT!
    , bitfield :: Word8 -- ^ set to true if the two values below are used
    , width :: Word8 -- ^ used for bf* instructions
    , offset :: Word8 -- ^ used for bf* instructions
    , indexSize :: Word8 -- ^ 0 = w, 1 = l
    } deriving (Show, Eq)

instance Storable M68KOpMemStruct where
    sizeOf _ = {#sizeof m68k_op_mem#}
    alignment _ = {#alignof m68k_op_mem#}
    peek p = M68KOpMemStruct
        <$> ((toEnum . fromIntegral) <$> {#get m68k_op_mem->base_reg#} p)
        <*> ((toEnum . fromIntegral) <$> {#get m68k_op_mem->index_reg#} p)
        <*> ((toEnum . fromIntegral) <$> {#get m68k_op_mem->in_base_reg#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->in_disp#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->out_disp#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->disp#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->scale#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->bitfield#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->width#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->offset#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->index_size#} p)
    poke p (M68KOpMemStruct bR iR iBR iD oD d s b w o i) = do
        {#set m68k_op_mem->base_reg#} p (fromIntegral $ fromEnum bR)
        {#set m68k_op_mem->index_reg#} p (fromIntegral $ fromEnum iR)
        {#set m68k_op_mem->in_base_reg#} p (fromIntegral $ fromEnum iBR)
        {#set m68k_op_mem->in_disp#} p (fromIntegral iD)
        {#set m68k_op_mem->out_disp#} p (fromIntegral oD)
        {#set m68k_op_mem->disp#} p (fromIntegral d)
        {#set m68k_op_mem->scale#} p (fromIntegral s)
        {#set m68k_op_mem->bitfield#} p (fromIntegral b)
        {#set m68k_op_mem->width#} p (fromIntegral w)
        {#set m68k_op_mem->offset#} p (fromIntegral o)
        {#set m68k_op_mem->index_size#} p (fromIntegral i)

-- | instruction operand value
data CsM68KOpValue
    = Imm Word64 -- ^ immediate value for `M68KOpImm` operand
    | Dimm Double -- ^ immediate double precision floating point value
    | Simm Float -- ^ immediate single precision floating point value
    | Reg M68KReg -- ^ register value for `M68KOpReg` operand
    | Mem M68KOpMemStruct -- ^ memory value for `M68KOpMem` operand
    | RegBits Word32 -- ^ register bits for movem/cas2/etc (always in d0-d7,
                     -- a0-a7, fp0-fp7 order)
    | RegPair M68KReg M68KReg -- ^ register pair in the same operand
                              -- (upper 4 bits first,
                              -- lower 4 bits second register)
    | Undefined -- ^ error value for `M68KOpInvalid` operand
    deriving (Show, Eq)

data CsM68KOp = CsM68KOp
    { value :: CsM68KOpValue
    , address_mode :: M68KAddressMode
    } deriving (Show, Eq)

instance Storable CsM68KOp where
    sizeOf _ = 40
    alignment _ = 8
    peek p = CsM68KOp
        <$> do
            t <- fromIntegral <$> (peekByteOff p 28 :: IO Int32)
            let bP = castPtr p
            case toEnum t of
              M68kOpImm -> Imm <$> (peek bP :: IO Word64)
              M68kOpFpDouble -> Dimm <$> peek bP
              M68kOpFpSingle -> Simm <$> peek bP
              M68kOpReg -> (Reg . toEnum . fromIntegral) <$>
                  (peek bP :: IO Int32)
              M68kOpMem -> Mem <$> (peek bP :: IO M68KOpMemStruct)
              M68kOpRegBits -> RegBits <$> peek bP
              M68kOpRegPair -> RegPair <$>
                  ((toEnum . fromIntegral) <$>
                      (peek bP :: IO Int32)) <*>
                  ((toEnum . fromIntegral) <$>
                      (peek (plusPtr bP 4) :: IO Int32))
              _ -> return Undefined
        <*> ((toEnum . fromIntegral) <$> (peekByteOff p 32 :: IO Int32))
    poke p (CsM68KOp v a) = do
        let bP = castPtr p
            setType t = pokeByteOff p 28 (fromIntegral $ fromEnum t :: Int32)
        case v of
          Imm i -> do
              poke bP i
              setType M68kOpImm
          Dimm d -> do
              poke bP d
              setType M68kOpFpDouble
          Simm s -> do
              poke bP s
              setType M68kOpFpSingle
          Reg r -> do
              poke bP (fromIntegral $ fromEnum r :: Int32)
              setType M68kOpReg
          Mem m -> do
              poke bP m
              setType M68kOpMem
          RegBits b -> do
              poke bP b
              setType M68kOpRegBits
          RegPair r0 r1 -> do
              poke bP (fromIntegral $ fromEnum r0 :: Int32)
              poke (plusPtr bP 4) (fromIntegral $ fromEnum r1 :: Int32)
              setType M68kOpRegPair
          _ -> setType M68kOpInvalid
        pokeByteOff p 32 (fromIntegral $ fromEnum a :: Int32)

-- | operation size of a CPU instruction
{#enum m68k_cpu_size as M68KCpuSize {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | operation size of a FPU instruction
{#enum m68k_fpu_size as M68KFpuSize {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | size type of an instruction
{#enum m68k_size_type as M68KSizeType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | operation size of an instruction (not it's own size)
data M68KOpSize
    = Cpu M68KCpuSize
    | Fpu M68KFpuSize
    deriving (Show, Eq)

instance Storable M68KOpSize where
    sizeOf _ = 8
    alignment _ = 4
    peek p = do
        t <- fromIntegral <$> {#get m68k_op_size->type#} p
        case toEnum t of
          M68kSizeTypeCpu -> (Cpu . toEnum . fromIntegral) <$>
              (peekByteOff p 4 :: IO Int32)
          M68kSizeTypeFpu -> (Fpu . toEnum . fromIntegral) <$>
              (peekByteOff p 4 :: IO Int32)
    poke p (Cpu c) = do
        poke (castPtr p) (fromIntegral $ fromEnum c :: Int32)
        poke (plusPtr p 4) (fromIntegral $ fromEnum M68kSizeTypeCpu :: Int32)
    poke p (Fpu f) = do
        poke (castPtr p) (fromIntegral $ fromEnum f :: Int32)
        poke (plusPtr p 4) (fromIntegral $ fromEnum M68kSizeTypeFpu :: Int32)

-- | a M68K instruction and it's operands
data CsM68K = CsM68K
    { operands :: [CsM68KOp]
    , op_size :: M68KOpSize
    } deriving (Show, Eq)

instance Storable CsM68K where
    sizeOf _ = 156
    alignment _ = 8
    peek p = CsM68K
        <$> do num <- fromIntegral <$> {#get cs_m68k->op_count#} p
               peekArray num (castPtr p)
        <*> (peek (plusPtr p 144) :: IO M68KOpSize)
    poke p (CsM68K o s) = do
        poke (plusPtr p 144) s
        {#set cs_m68k->op_count#} p (fromIntegral $ length o)
        if length o > 4
           then error "operands overflew 4 elements"
           else pokeArray (castPtr p) o

-- | M68K instructions
{#enum m68k_insn as M68KInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | M68K instruction groups
{#enum m68k_group_type as M68KGroupType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
