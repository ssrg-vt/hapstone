{-# LANGUAGE ForeignFunctionInterface #-}
module Hapstone.Internal.Sparc where

#include <capstone/sparc.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

{#enum sparc_cc as SparcCc {underscoreToCase} deriving (Show)#}
{#enum sparc_hint as SparcHint {underscoreToCase} deriving (Show)#}

{#enum sparc_op_type as SparcOpType {underscoreToCase} deriving (Show)#}

data SparcOpMemStruct = SparcOpMemStruct Word8 Word8 Int32

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

-- TODO: port cs_sparc_op struct
-- TODO: port cs_sparc struct

{#enum sparc_reg as SparcReg {underscoreToCase} deriving (Show)#}
{#enum sparc_insn as SparcInsn {underscoreToCase} deriving (Show)#}
{#enum sparc_insn_group as SparcInsnGroup {underscoreToCase} deriving (Show)#}
