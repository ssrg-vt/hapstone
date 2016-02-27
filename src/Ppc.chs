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
    peek p = PpcOpMemStruct <$>
        ((toEnum . fromIntegral) <$> peek (basePtr p)) <*>
        (fromIntegral <$> peek (dispPtr p))
    poke p (PpcOpMemStruct b d) = do
        poke (basePtr p) (fromIntegral $ fromEnum b)
        poke (dispPtr p) (fromIntegral d)

-- TODO: helper file
basePtr:: Ptr PpcOpMemStruct -> Ptr CUInt
basePtr p = plusPtr p {#offsetof ppc_op_mem.base#}
dispPtr :: Ptr PpcOpMemStruct -> Ptr CInt
dispPtr p = plusPtr p {#offsetof ppc_op_mem.disp#}

data PpcOpCrxStruct = PpcOpCrxStruct CUInt PpcReg PpcBc

instance Storable PpcOpCrxStruct where
    sizeOf _ = {#sizeof ppc_op_crx#}
    alignment _ = {#alignof ppc_op_crx#}
    peek p = PpcOpCrxStruct <$> (fromIntegral <$> peek (scalePtr p)) <*>
        ((toEnum . fromIntegral) <$> peek (regPtr p)) <*>
        ((toEnum . fromIntegral) <$> peek (condPtr p))
    poke p (PpcOpCrxStruct s r c) = do
        poke (scalePtr p) (fromIntegral s)
        poke (regPtr p) (fromIntegral $ fromEnum r)
        poke (condPtr p) (fromIntegral $ fromEnum r)

-- TODO: helper file, also types for enums ?!
scalePtr :: Ptr PpcOpCrxStruct -> Ptr CUInt
scalePtr p = plusPtr p {#offsetof ppc_op_crx.scale#}
regPtr :: Ptr PpcOpCrxStruct -> Ptr CUInt
regPtr p = plusPtr p {#offsetof ppc_op_crx.reg#}
condPtr :: Ptr PpcOpCrxStruct -> Ptr CUInt
condPtr p = plusPtr p {#offsetof ppc_op_crx.cond#}

-- TODO: port cs_ppc_op struct
-- TODO: port cs_ppc struct

{#enum ppc_insn as PpcInsn {underscoreToCase} deriving (Show)#}
{#enum ppc_insn_group as PpcInsnGroup {underscoreToCase} deriving (Show)#}
