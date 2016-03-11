{-# LANGUAGE ForeignFunctionInterface #-}
module Hapstone.Internal.SystemZ where

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

data CsSysZOp
    = Reg Word32
    | Imm Int64
    | Mem SysZOpMemStruct
    | AcReg
    | Undefined

instance Storable CsSysZOp where
    sizeOf _ = {#sizeof cs_sysz_op#}
    alignment _ = {#alignof cs_sysz_op#}
    peek p = do
        t <- fromIntegral <$> {#get cs_sysz_op->type#} p
        let bP = plusPtr p -- FIXME: maybe alignment will bite us!
               ({#offsetof cs_sysz_op.type#} + {#sizeof sysz_op_type#})
        case toEnum t of
          SyszOpReg -> Reg <$> peek bP
          SyszOpImm -> Imm <$> peek bP
          SyszOpMem -> Mem <$> peek bP
          SyszOpAcreg -> return AcReg
          SyszOpInvalid -> return Undefined
    poke p op = do
        let bP = plusPtr p -- FIXME: maybe alignment will bite us!
               ({#offsetof cs_sysz_op.type#} + {#sizeof sysz_op_type#})
            setType = {#set cs_sysz_op->type#} p . fromIntegral . fromEnum
        case op of
          Reg r -> do
              poke bP (fromIntegral $ fromEnum r :: CInt)
              setType SyszOpReg
          Imm i -> do
              poke bP i
              setType SyszOpImm
          Mem m -> do
              poke bP m
              setType SyszOpMem
          AcReg -> setType SyszOpAcreg
          _ -> setType SyszOpInvalid

data CsSysZ = CsSysZ
    { cc :: SysZCc
    , operands :: [CsSysZOp]
    }

instance Storable CsSysZ where
    sizeOf _ = {#sizeof cs_sysz#}
    alignment _ = {#alignof cs_sysz#}
    peek p = CsSysZ
        <$> ((toEnum . fromIntegral) <$> {#get cs_sysz->cc#} p)
        <*> do num <- fromIntegral <$> {#get cs_sysz->op_count#} p
               let ptr = plusPtr p {#offsetof cs_sysz.operands#}
               peekArray num ptr
    poke p (CsSysZ cc o) = do
        {#set cs_sysz->cc#} p (fromIntegral $ fromEnum cc)
        {#set cs_sysz->op_count#} p (fromIntegral $ length o)
        if length o > 6
           then error "operands overflew 6 elements"
           else pokeArray (plusPtr p {#offsetof cs_sysz->operands#}) o

{#enum sysz_reg as SyszReg {underscoreToCase} deriving (Show)#}
{#enum sysz_insn as SyszInsn {underscoreToCase} deriving (Show)#}
{#enum sysz_insn_group as SyszInsnGroup {underscoreToCase} deriving (Show)#}
