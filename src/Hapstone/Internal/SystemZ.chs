{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.SystemZ
Description : SystemZ architecture header ported using C2HS + some boilerplate
Copyright   : (c) Inokentiy Babushkin, 2016
License     : BSD3
Maintainer  : Inokentiy Babushkin <inokentiy.babushkin@googlemail.com>
Stability   : experimental

This module contains SystemZ specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.SystemZ where

#include <capstone/systemz.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

-- | SystemZ condition code
{#enum sysz_cc as SysZCc {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | operand type for instruction's operands
{#enum sysz_op_type as SysZOpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | SystemZ registers
{#enum sysz_reg as SysZReg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | memory access operands
data SysZOpMemStruct = SysZOpMemStruct
    { base :: Word8 -- ^ base register
    , index :: Word8 -- ^ index register
    , len :: Word64 -- ^ BDLAddr operand
    , disp :: Int64 -- ^ displacement/offset value
    } deriving (Show, Eq)

instance Storable SysZOpMemStruct where
    sizeOf _ = {#sizeof sysz_op_mem#}
    alignment _ = {#alignof sysz_op_mem#}
    peek p = SysZOpMemStruct
        <$> (fromIntegral <$> {#get sysz_op_mem->base#} p)
        <*> (fromIntegral <$> {#get sysz_op_mem->index#} p)
        <*> (fromIntegral <$> {#get sysz_op_mem->length#} p)
        <*> (fromIntegral <$> {#get sysz_op_mem->disp#} p)
    poke p (SysZOpMemStruct b i l d) = do
        {#set sysz_op_mem->base#} p (fromIntegral b)
        {#set sysz_op_mem->index#} p (fromIntegral i)
        {#set sysz_op_mem->length#} p (fromIntegral l)
        {#set sysz_op_mem->disp#} p (fromIntegral d)

-- | instruction operand
data CsSysZOp
    = Reg Word32 -- ^ register value for 'SyszOpReg' operands
    | Imm Int64 -- ^ immediate value for 'SyszOpImm' operands
    | Mem SysZOpMemStruct -- ^ base/index/length/disp value for 'SyszOpMem'
                          -- operands
    | AcReg -- ^ 'SyszOpAcreg' operand
    | Undefined -- ^ invalid operand value, for 'Arm64OpInvalid' operand
    deriving (Show, Eq)

instance Storable CsSysZOp where
    sizeOf _ = 32
    alignment _ = 8
    peek p = do
        t <- fromIntegral <$> {#get cs_sysz_op->type#} p
        let bP = plusPtr p 8
        case toEnum t of
          SyszOpReg -> Reg <$> peek bP
          SyszOpImm -> Imm <$> peek bP
          SyszOpMem -> Mem <$> peek bP
          SyszOpAcreg -> return AcReg
          SyszOpInvalid -> return Undefined
    poke p op = do
        let bP = plusPtr p 8
            setType = {#set cs_sysz_op->type#} p . fromIntegral . fromEnum
        case op of
          Reg r -> do
              poke bP (fromIntegral $ fromEnum r :: CInt)
              setType SyszOpReg
          Imm i -> do
              poke bP i
              setType SyszOpImm
          Mem m -> do
              poke bP m
              setType SyszOpMem
          AcReg -> setType SyszOpAcreg
          _ -> setType SyszOpInvalid

-- | instruction datatype
data CsSysZ = CsSysZ
    { cc :: SysZCc -- ^ condition code
    , operands :: [CsSysZOp] -- operand list of this instruction, *MUST* have
                             -- <= 6 elements, else you'll get a runtime error
                             -- when you (implicitly) try to write it to memory
                             -- via it's Storable instance
    } deriving (Show, Eq)

instance Storable CsSysZ where
    sizeOf _ = 200
    alignment _ = 8
    peek p = CsSysZ
        <$> ((toEnum . fromIntegral) <$> {#get cs_sysz->cc#} p)
        <*> do num <- fromIntegral <$> {#get cs_sysz->op_count#} p
               let ptr = plusPtr p {#offsetof cs_sysz.operands#}
               peekArray num ptr
    poke p (CsSysZ cc o) = do
        {#set cs_sysz->cc#} p (fromIntegral $ fromEnum cc)
        {#set cs_sysz->op_count#} p (fromIntegral $ length o)
        if length o > 6
           then error "operands overflowed 6 elements"
           else pokeArray (plusPtr p {#offsetof cs_sysz->operands#}) o

-- | SystemZ instructions
{#enum sysz_insn as SysZInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | SystemZ instruction groups

{#enum sysz_insn_group as SysZInsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
