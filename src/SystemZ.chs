{-# LANGUAGE ForeignFunctionInterface #-}
module SystemZ where

#include <capstone/systemz.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

{#enum sysz_cc as SysZCc {underscoreToCase} deriving (Show)#}

{#enum sysz_op_type as SysZOpType {underscoreToCase} deriving (Show)#}

data SysZOpMemStruct = SysZOpMemStruct Word8 Word8 Word64 Int64

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

-- TODO: port cs_sysz_op struct
-- TODO: port cs_sysz struct

{#enum sysz_reg as SyszReg {underscoreToCase} deriving (Show)#}
{#enum sysz_insn as SyszInsn {underscoreToCase} deriving (Show)#}
{#enum sysz_insn_group as SyszInsnGroup {underscoreToCase} deriving (Show)#}
