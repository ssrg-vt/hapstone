{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.Sparc
Description : SPARC architecture header ported using C2HS + some boilerplate
Copyright   : (c) Inokentiy Babushkin, 2016
License     : BSD3
Maintainer  : Inokentiy Babushkin <inokentiy.babushkin@googlemail.com>
Stability   : experimental

This module contains SPARC specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.Sparc where

#include <capstone/sparc.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

-- | SPARC condition codes
{#enum sparc_cc as SparcCc {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | SPARC branch hint
{#enum sparc_hint as SparcHint {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | operand type for instruction's operands
{#enum sparc_op_type as SparcOpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | memory access operand
-- associated with 'SparcOpMem' operand type
data SparcOpMemStruct = SparcOpMemStruct
    { base :: Word8 -- ^ base register
    , index :: Word8 -- ^ index register
    , disp :: Int32 -- ^ displacement/offset value
    } deriving (Show, Eq)

instance Storable SparcOpMemStruct where
    sizeOf _ = {#sizeof sparc_op_mem#}
    alignment _ = {#alignof sparc_op_mem#}
    peek p = SparcOpMemStruct
        <$> (fromIntegral <$> {#get sparc_op_mem->base#} p)
        <*> (fromIntegral <$> {#get sparc_op_mem->index#} p)
        <*> (fromIntegral <$> {#get sparc_op_mem->disp#} p)
    poke p (SparcOpMemStruct b i d) = do
        {#set sparc_op_mem->base#} p (fromIntegral b)
        {#set sparc_op_mem->index#} p (fromIntegral i)
        {#set sparc_op_mem->disp#} p (fromIntegral d)

-- | instruction operand
data CsSparcOp
    = Reg Word32 -- ^ register value for 'SparcOpReg' operands
    | Imm Int32 -- ^ immediate value for 'SparcOpImm' operands
    | Mem SparcOpMemStruct -- ^ base,index,disp value for 'SparcOpMem' operands
    | Undefined -- ^ invalid operand value, for 'SparcOpInvalid' operand
    deriving (Show, Eq)

instance Storable CsSparcOp where
    sizeOf _ = 12
    alignment _ = 4
    peek p = do
        t <- fromIntegral <$> ({#get cs_sparc_op->type#} p :: IO CInt)
        let bP = plusPtr p 4
        case toEnum t of
          SparcOpReg -> Reg <$> peek bP
          SparcOpImm -> Imm <$> peek bP
          SparcOpMem -> Mem <$> peek bP
          _ -> return Undefined
    poke p op = do
        let bP = plusPtr p 4
            setType = {#set cs_sparc_op->type#} p . fromIntegral . fromEnum
        case op of
          Reg r -> do
              poke bP (fromIntegral $ fromEnum r :: CInt)
              setType SparcOpReg
          Imm i -> do
              poke bP i
              setType SparcOpImm
          Mem m -> do
              poke bP m
              setType SparcOpMem
          _ -> setType SparcOpInvalid

-- | instruction datatype
data CsSparc = CsSparc
    { cc :: SparcCc -- ^ condition code
    , hint :: SparcHint -- ^ branch hint
    , operands :: [CsSparcOp] -- ^ operand list of this instruction, *MUST*
                              -- have <= 4 elements, else you'll get a runtime
                              -- error when you (implicitly) try to write it to
                              -- memory via it's Storable instance
    } deriving (Show, Eq)

instance Storable CsSparc where
    sizeOf _ = 60
    alignment _ = 4
    peek p = CsSparc
        <$> ((toEnum . fromIntegral) <$> {#get cs_sparc->cc#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_sparc->hint#} p)
        <*> do num <- fromIntegral <$> {#get cs_sparc->op_count#} p
               let ptr = plusPtr p {#offsetof cs_sparc.operands#}
               peekArray num ptr
    poke p (CsSparc cc h o) = do
        {#set cs_sparc->cc#} p (fromIntegral $ fromEnum cc)
        {#set cs_sparc->hint#} p (fromIntegral $ fromEnum h)
        {#set cs_sparc->op_count#} p (fromIntegral $ length o)
        if length o > 4
           then error "operands overflew 4 elements"
           else pokeArray (plusPtr p {#offsetof cs_sparc->operands#}) o

-- | SPARC registers
{#enum sparc_reg as SparcReg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | SPARC instructions
{#enum sparc_insn as SparcInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | SPARC instruction groups
{#enum sparc_insn_group as SparcInsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
