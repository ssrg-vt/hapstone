{-# LANGUAGE ForeignFunctionInterface #-}
module Mips where

#include <capstone/mips.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

{#enum mips_op_type as MipsOpType {underscoreToCase} deriving (Show)#}

-- TODO: high level types
data MipsOpMemStruct = MipsOpMemStruct CUInt Int64

instance Storable MipsOpMemStruct where
    sizeOf _ = {#sizeof mips_op_mem#}
    alignment _ = {#alignof mips_op_mem#}
    peek p = MipsOpMemStruct <$> (fromIntegral <$> peek (basePtr p)) <*>
        (fromIntegral <$> peek (dispPtr p))
    poke p (MipsOpMemStruct b d) = do
        poke (basePtr p) (fromIntegral b)
        poke (dispPtr p) (fromIntegral d)

-- TODO: helper file
basePtr :: Ptr MipsOpMemStruct -> Ptr CUInt
basePtr p = plusPtr p {#offsetof mips_op_mem.base#}
dispPtr :: Ptr MipsOpMemStruct -> Ptr CInt
dispPtr p = plusPtr p {#offsetof mips_op_mem.disp#}

-- TODO: port cs_mips_op struct
-- TODO: port cs_mips struct

{#enum mips_reg as MipsReg {underscoreToCase} deriving (Show)#}
{#enum mips_insn as MipsInsn {underscoreToCase} deriving (Show)#}
{#enum mips_insn_group as MipsInsnGroup {underscoreToCase} deriving (Show)#}
