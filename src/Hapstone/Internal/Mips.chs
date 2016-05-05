{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.Mips
Description : MIPS architecture header ported using C2HS + some boilerplate
Copyright   : (c) Inokentiy Babushkin, 2016
License     : BSD3
Maintainer  : Inokentiy Babushkin <inokentiy.babushkin@googlemail.com>
Stability   : experimental

This module contains MIPS specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.Mips where

#include <capstone/mips.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

-- | operand type for instruction's operands
{#enum mips_op_type as MipsOpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | MIPS registers
{#enum mips_reg as MipsReg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | memory access operands
-- associated with 'MipsOpMem' operand type
data MipsOpMemStruct = MipsOpMemStruct 
    { base :: MipsReg -- ^ base register
    , disp ::  Int64 -- ^ displacement/offset value
    } deriving (Show, Eq)

instance Storable MipsOpMemStruct where
    sizeOf _ = {#sizeof mips_op_mem#}
    alignment _ = {#alignof mips_op_mem#}
    peek p = MipsOpMemStruct
        <$> ((toEnum . fromIntegral) <$> {#get mips_op_mem->base#} p)
        <*> (fromIntegral <$> {#get mips_op_mem->disp#} p)
    poke p (MipsOpMemStruct b d) = do
        {#set mips_op_mem->base#} p (fromIntegral $ fromEnum b)
        {#set mips_op_mem->disp#} p(fromIntegral d)

-- | instruction operand
data CsMipsOp
    = Reg Word32 -- ^ register value for 'MipsOpReg' operands
    | Imm Int64 -- ^ immediate value for 'MipsOpImm' operands
    | Mem MipsOpMemStruct -- ^ base,disp value for 'MipsOpMem' operands
    | Undefined -- ^ invalid operand value, for MipsOpInvalid operand
    deriving (Show, Eq)

instance Storable CsMipsOp where
    sizeOf _ = 24
    alignment _ = 8
    peek p = do
        t <- fromIntegral <$> {#get cs_mips_op->type#} p
        let bP = plusPtr p 8
        case toEnum t of
          MipsOpReg -> (Reg . fromIntegral) <$> (peek bP :: IO CUInt)
          MipsOpImm -> (Imm . fromIntegral) <$> (peek bP :: IO Int64)
          MipsOpMem -> Mem <$> (peek bP :: IO MipsOpMemStruct)
          _ -> return Undefined
    poke p op = do
        let bP = plusPtr p 8
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

-- | instruction datatype
newtype CsMips = CsMips [CsMipsOp] -- ^ operand list for this instruction,
                                   -- *MUST* have <= 8 elements, else you'll
                                   -- get a runtime error when you (implicitly)
                                   -- try to write it to memory via it's
                                   -- Storable instance
    deriving (Show, Eq)

instance Storable CsMips where
    sizeOf _ = 200
    alignment _ = 8
    peek p = CsMips
        <$> do num <- fromIntegral <$> {#get cs_mips->op_count#} p
               let ptr = plusPtr p 8
               peekArray num ptr
    poke p (CsMips o) = do
        {#set cs_mips->op_count#} p (fromIntegral $ length o)
        if length o > 8
           then error "operands overflew 8 elements"
           else pokeArray (plusPtr p 8) o

-- | MIPS instructions
{#enum mips_insn as MipsInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | MIPS instruction groups
{#enum mips_insn_group as MipsInsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
