{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.Arm64 where

#include <capstone/arm64.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

{#enum arm64_shifter as Arm64Shifter {underscoreToCase} deriving (Show)#}
{#enum arm64_extender as Arm64Extender {underscoreToCase} deriving (Show)#}
{#enum arm64_cc as Arm64ConditionCode {underscoreToCase} deriving (Show)#}

{#enum arm64_sysreg as Arm64Sysreg {underscoreToCase} deriving (Show)#}
{#enum arm64_msr_reg as Arm64MsrReg {underscoreToCase} deriving (Show)#}

{#enum arm64_pstate as Arm64Pstate {underscoreToCase} deriving (Show)#}

{#enum arm64_vas as Arm64Vas {underscoreToCase} deriving (Show)#}
{#enum arm64_vess as Arm64Vess {underscoreToCase} deriving (Show)#}

{#enum arm64_barrier_op as Arm64BarrierOp {underscoreToCase} deriving (Show)#}
{#enum arm64_op_type as Arm64OpType {underscoreToCase} deriving (Show)#}
{#enum arm64_tlbi_op as Arm64TlbiOp {underscoreToCase} deriving (Show)#}
{#enum arm64_at_op as Arm64AtOp {underscoreToCase} deriving (Show)#}
{#enum arm64_dc_op as Arm64DcOp {underscoreToCase} deriving (Show)#}
{#enum arm64_ic_op as Arm64IcOp {underscoreToCase} deriving (Show)#}
{#enum arm64_prefetch_op as Arm64PrefetchOp
    {underscoreToCase} deriving (Show)#}

-- TODO: high level types
data Arm64OpMemStruct = Arm64OpMemStruct Word32 Word32 Int32

instance Storable Arm64OpMemStruct where
    sizeOf _ = {#sizeof arm64_op_mem#}
    alignment _ = {#alignof arm64_op_mem#}
    peek p = Arm64OpMemStruct
        <$> (fromIntegral <$> {#get arm64_op_mem->base#} p)
        <*> (fromIntegral <$> {#get arm64_op_mem->index#} p)
        <*> (fromIntegral <$> {#get arm64_op_mem->disp#} p)
    poke p (Arm64OpMemStruct b i d) = do
        {#set arm64_op_mem->base#} p (fromIntegral b)
        {#set arm64_op_mem->index#} p (fromIntegral i)
        {#set arm64_op_mem->disp#} p (fromIntegral d)

-- TODO: port cs_arm64_op
-- TODO: port cs_arm64

{#enum arm64_reg as Arm64Reg {underscoreToCase} deriving (Show)#}
{#enum arm64_insn as Arm64Insn {underscoreToCase} deriving (Show)#}
{#enum arm64_insn_group as Arm64InsnGroup {underscoreToCase} deriving (Show)#}
