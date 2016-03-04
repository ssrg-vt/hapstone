{-# LANGUAGE ForeignFunctionInterface #-}
module Hapstone.Internal.Mips where

#include <capstone/mips.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

{#enum mips_op_type as MipsOpType {underscoreToCase} deriving (Show)#}

data MipsOpMemStruct = MipsOpMemStruct Word32 Int64

instance Storable MipsOpMemStruct where
    sizeOf _ = {#sizeof mips_op_mem#}
    alignment _ = {#alignof mips_op_mem#}
    peek p = MipsOpMemStruct
        <$> (fromIntegral <$> {#get mips_op_mem->base#} p)
        <*> (fromIntegral <$> {#get mips_op_mem->disp#} p)
    poke p (MipsOpMemStruct b d) = do
        {#set mips_op_mem->base#} p (fromIntegral b)
        {#set mips_op_mem->disp#} p(fromIntegral d)

data CsMipsOp
    = Reg Word32
    | Imm Int64
    | Mem MipsOpMemStruct
    | Undefined

instance Storable CsMipsOp where
    sizeOf _ = {#sizeof cs_mips_op#}
    alignment _ = {#alignof cs_mips_op#}
    peek p = do
        t <- fromIntegral <$> {#get cs_mips_op->type#} p
        let bP = plusPtr p -- FIXME: maybe alignment will bite us!
               ({#offsetof cs_mips_op.type#} + {#sizeof mips_op_type#})
        case toEnum t :: MipsOpType of
          MipsOpReg -> (Reg . fromIntegral) <$> (peek bP :: IO CUInt)
          MipsOpImm -> (Imm . fromIntegral) <$> (peek bP :: IO Int64)
          MipsOpMem -> Mem <$> (peek bP :: IO MipsOpMemStruct)
          _ -> return Undefined
    poke p op = do
        let bP = plusPtr p -- FIXME: maybe alignment will bite us!
               ({#offsetof cs_mips_op.type#} + {#sizeof mips_op_type#})
            setType = {#set cs_mips_op->type#} p . fromIntegral . fromEnum
        case op of
          Reg r -> do
              poke bP (fromIntegral r :: CUInt)
              setType MipsOpReg
          Imm i -> do
              poke bP (fromIntegral i :: Int64)
              setType MipsOpImm
          Mem m -> do
              poke bP m
              setType MipsOpMem
          _ -> setType MipsOpInvalid

newtype CsMips = CsMips [CsMipsOp]

instance Storable CsMips where
    sizeOf _ = {#sizeof cs_mips#}
    alignment _ = {#alignof cs_mips#}
    peek p = CsMips
        <$> do num <- fromIntegral <$> {#get cs_mips->op_count#} p
               let ptr = plusPtr p {#offsetof cs_mips.operands#}
               peekArray num ptr
    poke p (CsMips o) = do
        {#set cs_mips->op_count#} p (fromIntegral $ length o)
        if length o > 8
           then error "operands overflew 8 elements"
           else pokeArray (plusPtr p {#offsetof cs_mips->operands#}) o

{#enum mips_reg as MipsReg {underscoreToCase} deriving (Show)#}
{#enum mips_insn as MipsInsn {underscoreToCase} deriving (Show)#}
{#enum mips_insn_group as MipsInsnGroup {underscoreToCase} deriving (Show)#}
