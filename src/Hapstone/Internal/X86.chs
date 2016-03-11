{-# LANGUAGE ForeignFunctionInterface #-}
module Hapstone.Internal.X86 where

-- ugly workaround because... capstone doesn't import stdbool.h
#include <stdbool.h> 
#include <capstone/x86.h>

{#context lib = "capstone"#}

import Data.Maybe (fromMaybe)

import Foreign
import Foreign.C.Types

{#enum x86_reg as X86Reg {underscoreToCase} deriving (Show)#}

{#enum x86_op_type as X86OpType {underscoreToCase} deriving (Show)#}

{#enum x86_avx_bcast as X86AvxBcast {underscoreToCase} deriving (Show)#}
{#enum x86_sse_cc as X86SseCc {underscoreToCase} deriving (Show)#}
{#enum x86_avx_cc as X86AvxCc {underscoreToCase} deriving (Show)#}
{#enum x86_avx_rm as X86AvxRm {underscoreToCase} deriving (Show)#}

{#enum x86_prefix as X86Prefix {underscoreToCase} deriving (Show)#}

data X86OpMemStruct = X86OpMemStruct Word32 Word32 Word32 Int32 Int64

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

data CsX86OpValue
    = Reg X86Reg
    | Imm Word64
    | Fp Double
    | Mem X86OpMemStruct
    | Undefined

data CsX86Op = CsX86Op
    { value :: CsX86OpValue
    , size :: Word8
    , avxBcast :: Maybe X86AvxBcast
    , avxZeroOpmask :: Bool
    }

instance Storable CsX86Op where
    sizeOf _ = {#sizeof cs_x86_op#}
    alignment _ = {#alignof cs_x86_op#}
    peek p = CsX86Op
        <$> do
            t <- fromIntegral <$> {#get cs_x86_op->type#} p
            let bP = plusPtr p -- FIXME: maybe alignment will bite us!
                   ({#offsetof cs_x86_op.type#} + {#sizeof x86_op_type#})
            case toEnum t of
              X86OpReg -> (Reg . toEnum . fromIntegral) <$>
                  (peek bP :: IO CInt)
              X86OpImm -> Imm <$> peek bP
              X86OpFp -> (Fp . realToFrac) <$> (peek bP :: IO CDouble)
              X86OpMem -> Mem <$> peek bP
              _ -> return Undefined
        <*> (fromIntegral <$> {#get cs_x86_op->size#} p)
        <*> do bc <- fromIntegral <$> ({#get cs_x86_op->avx_bcast#} p)
               if bc == 0
                  then return Nothing
                  else return . Just $ toEnum bc
        <*> ({#get cs_x86_op->avx_zero_opmask#} p)
    poke p (CsX86Op val s ab az) = do
        let bP = plusPtr p -- FIXME: maybe alignment will bite us!
               ({#offsetof cs_x86_op.type#} + {#sizeof x86_op_type#})
            setType = {#set cs_x86_op->type#} p . fromIntegral . fromEnum
        case val of
          Reg r -> do
              poke bP (fromIntegral $ fromEnum r :: CInt)
              setType X86OpReg
          Imm i -> do
              poke bP i
              setType X86OpImm
          Fp f -> do
              poke bP (realToFrac f :: CDouble)
              setType X86OpFp
          Mem m -> do
              poke bP m
              setType X86OpMem
          Undefined -> setType X86OpInvalid
        {#set cs_x86_op->size#} p (fromIntegral s)
        {#set cs_x86_op->avx_bcast#} p
            (fromIntegral . fromMaybe 0 $ fromEnum <$> ab :: CInt)
        {#set cs_x86_op->avx_zero_opmask#} p az
    
-- TODO: port cs_x86 struct

{#enum x86_insn as X86Insn {underscoreToCase} deriving (Show)#}
{#enum x86_insn_group as X86InsnGroup {underscoreToCase} deriving (Show)#}
