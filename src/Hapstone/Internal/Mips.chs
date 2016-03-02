{-# LANGUAGE ForeignFunctionInterface #-}
module Hapstone.Internal.Mips where

#include <capstone/mips.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

{#enum mips_op_type as MipsOpType {underscoreToCase} deriving (Show)#}

-- TODO: high level types
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

-- TODO: port cs_mips_op struct
-- TODO: port cs_mips struct

{#enum mips_reg as MipsReg {underscoreToCase} deriving (Show)#}
{#enum mips_insn as MipsInsn {underscoreToCase} deriving (Show)#}
{#enum mips_insn_group as MipsInsnGroup {underscoreToCase} deriving (Show)#}
