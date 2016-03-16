{-# LANGUAGE ForeignFunctionInterface #-}
module Hapstone.Internal.XCore where

#include <capstone/xcore.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

-- enumeration(s)
{#enum xcore_op_type as XCoreOpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- memory access operands
data XCoreOpMemStruct = XCoreOpMemStruct Word8 Word8 Int32 Int32
    deriving (Show, Eq)

instance Storable XCoreOpMemStruct where
    sizeOf _ = {#sizeof xcore_op_mem#}
    alignment _ = {#alignof xcore_op_mem#}
    peek p = XCoreOpMemStruct
        <$> (fromIntegral <$> {#get xcore_op_mem->base#} p)
        <*> (fromIntegral <$> {#get xcore_op_mem->index#} p)
        <*> (fromIntegral <$> {#get xcore_op_mem->disp#} p)
        <*> (fromIntegral <$> {#get xcore_op_mem->direct#} p)
    poke p (XCoreOpMemStruct b i disp dir) = do
        {#set xcore_op_mem->base#} p (fromIntegral b)
        {#set xcore_op_mem->index#} p (fromIntegral i)
        {#set xcore_op_mem->disp#} p (fromIntegral disp)
        {#set xcore_op_mem->direct#} p (fromIntegral dir)

-- operands
data CsXCoreOp
    = Reg Word32
    | Imm Int32
    | Mem XCoreOpMemStruct
    | Undefined
    deriving (Show, Eq)

instance Storable CsXCoreOp where
    sizeOf _ = 16
    alignment _ = 4
    peek p = do
        t <- fromIntegral <$> {#get cs_xcore_op->type#} p
        let bP = plusPtr p 4
        case toEnum t of
          XcoreOpReg -> Reg <$> peek bP
          XcoreOpImm -> Imm <$> peek bP
          XcoreOpMem -> Mem <$> peek bP
          _ -> return Undefined
    poke p op = do
        let bP = plusPtr p 4
            setType = {#set cs_xcore_op->type#} p . fromIntegral . fromEnum
        case op of
          Reg r -> poke bP r >> setType XcoreOpReg
          Imm i -> poke bP i >> setType XcoreOpImm
          Mem m -> poke bP m >> setType XcoreOpMem
          _ -> setType XcoreOpInvalid

-- instructions
newtype CsXCore = CsXCore [CsXCoreOp]
    deriving (Show, Eq)

instance Storable CsXCore where
    sizeOf _ = 132
    alignment _ = 4
    peek p = do
        num <- fromIntegral <$> {#get cs_xcore->op_count#} p
        CsXCore <$> peekArray num (plusPtr p {#offsetof cs_xcore.operands#})
    poke p (CsXCore o) = do
        {#set cs_xcore->op_count#} p (fromIntegral $ length o)
        if length o > 8
           then error "operands overflew 8 elements"
           else pokeArray (plusPtr p {#offsetof cs_xcore->operands#}) o

-- more enumerations
{#enum xcore_reg as XCoreReg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum xcore_insn as XCoreInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum xcore_insn_group as XCoreInsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
