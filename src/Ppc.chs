{-# LANGUAGE ForeignFunctionInterface #-}
module Ppc where

#include <capstone/ppc.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

{#enum ppc_bc as PpcBc {underscoreToCase} deriving (Show)#}
{#enum ppc_bh as PpcBh {underscoreToCase} deriving (Show)#}

{#enum ppc_reg as PpcReg {underscoreToCase} deriving (Show)#}

{#enum ppc_op_type as PpcOpType {underscoreToCase} deriving (Show)#}

-- TODO: high level types
data PpcOpMemStruct = PpcOpMemStruct PpcReg Int32

instance Storable PpcOpMemStruct where
    sizeOf _ = {#sizeof ppc_op_mem#}
    alignment _ = {#alignof ppc_op_mem#}
    peek p = PpcOpMemStruct
        <$> ((toEnum . fromIntegral) <$> {#get ppc_op_mem->base#} p)
        <*> (fromIntegral <$> {#get ppc_op_mem->disp#} p)
    poke p (PpcOpMemStruct b d) = do
        {#set ppc_op_mem->base#} p (fromIntegral $ fromEnum b)
        {#set ppc_op_mem->disp#} p (fromIntegral d)

data PpcOpCrxStruct = PpcOpCrxStruct CUInt PpcReg PpcBc

instance Storable PpcOpCrxStruct where
    sizeOf _ = {#sizeof ppc_op_crx#}
    alignment _ = {#alignof ppc_op_crx#}
    peek p = PpcOpCrxStruct
        <$> (fromIntegral <$> {#get ppc_op_crx->scale#} p)
        <*> ((toEnum . fromIntegral) <$> {#get ppc_op_crx->reg#} p)
        <*> ((toEnum . fromIntegral) <$> {#get ppc_op_crx->cond#} p)
    poke p (PpcOpCrxStruct s r c) = do
        {#set ppc_op_crx->scale#} p (fromIntegral s)
        {#set ppc_op_crx->reg#} p (fromIntegral $ fromEnum r)
        {#set ppc_op_crx->cond#} p (fromIntegral $ fromEnum r)

-- TODO: port cs_ppc_op struct
-- TODO: port cs_ppc struct

{#enum ppc_insn as PpcInsn {underscoreToCase} deriving (Show)#}
{#enum ppc_insn_group as PpcInsnGroup {underscoreToCase} deriving (Show)#}
