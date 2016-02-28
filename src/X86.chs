{-# LANGUAGE ForeignFunctionInterface #-}
module X86 where

-- ugly workaround because... capstone doesn't import stdbool.h
#include <stdbool.h> 
#include <capstone/x86.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

{#enum x86_reg as X86Reg {underscoreToCase} deriving (Show)#}

{#enum x86_op_type as X86OpType {underscoreToCase} deriving (Show)#}

{#enum x86_avx_bcast as X86AvxBcast {underscoreToCase} deriving (Show)#}
{#enum x86_sse_cc as X86SseCc {underscoreToCase} deriving (Show)#}
{#enum x86_avx_cc as X86AvxCc {underscoreToCase} deriving (Show)#}
{#enum x86_avx_rm as X86AvxRm {underscoreToCase} deriving (Show)#}

{#enum x86_prefix as X86Prefix {underscoreToCase} deriving (Show)#}

data X86OpMemStruct = X86OpMemStruct CUInt CUInt CUInt CInt Int64

instance Storable X86OpMemStruct where
    sizeOf _ = {#sizeof x86_op_mem#}
    alignment _ = {#alignof x86_op_mem#}
    peek p = X86OpMemStruct
        <$> (fromIntegral <$> {#get x86_op_mem->segment#} p)
        <*> (fromIntegral <$> {#get x86_op_mem->base#} p)
        <*> (fromIntegral <$> {#get x86_op_mem->index#} p)
        <*> (fromIntegral <$> {#get x86_op_mem->scale#} p)
        <*> (fromIntegral <$> {#get x86_op_mem->disp#} p)
    poke p (X86OpMemStruct se b i sc d) = do
        {#set x86_op_mem->segment#} p (fromIntegral se)
        {#set x86_op_mem->base#} p (fromIntegral b)
        {#set x86_op_mem->index#} p (fromIntegral i)
        {#set x86_op_mem->scale#} p (fromIntegral sc)
        {#set x86_op_mem->disp#} p (fromIntegral d)

-- TODO: port cs_x86_op struct
-- TODO: port cs_x86 struct

{#enum x86_insn as X86Insn {underscoreToCase} deriving (Show)#}
{#enum x86_insn_group as X86InsnGroup {underscoreToCase} deriving (Show)#}
