{-# LANGUAGE ForeignFunctionInterface #-}
module Hapstone.Internal.XCore where

#include <capstone/xcore.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

{#enum xcore_op_type as XCoreOpType {underscoreToCase} deriving (Show)#}

data XCoreOpMemStruct = XCoreOpMemStruct Word8 Word8 Int32 Int32

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

-- TODO: port cs_xcore_op struct
-- TODO: port cs_xcore struct

{#enum xcore_reg as XCoreReg {underscoreToCase} deriving (Show)#}
{#enum xcore_insn as XCoreInsn {underscoreToCase} deriving (Show)#}
{#enum xcore_insn_group as XCoreInsnGroup {underscoreToCase} deriving (Show)#}
