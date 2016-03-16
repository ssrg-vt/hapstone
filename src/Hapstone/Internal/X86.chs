{-# LANGUAGE ForeignFunctionInterface #-}
module Hapstone.Internal.X86 where

-- ugly workaround because... capstone doesn't import stdbool.h
#include <stdbool.h> 
#include <capstone/x86.h>

{#context lib = "capstone"#}

import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)

import Foreign
import Foreign.C.Types

import Hapstone.Internal.Util

-- enumerations
{#enum x86_reg as X86Reg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum x86_op_type as X86OpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum x86_avx_bcast as X86AvxBcast {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum x86_sse_cc as X86SseCc {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum x86_avx_cc as X86AvxCc {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum x86_avx_rm as X86AvxRm {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum x86_prefix as X86Prefix {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- memory access operands
data X86OpMemStruct = X86OpMemStruct Word32 Word32 Word32 Int32 Int64
    deriving (Show, Eq)

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

-- possible operand types
data CsX86OpValue
    = Reg X86Reg
    | Imm Word64
    | Fp Double
    | Mem X86OpMemStruct
    | Undefined
    deriving (Show, Eq)

-- operands
data CsX86Op = CsX86Op
    { value :: CsX86OpValue
    , size :: Word8
    , avxBcast :: X86AvxBcast
    , avxZeroOpmask :: Bool
    } deriving (Show, Eq)

instance Storable CsX86Op where
    sizeOf _ = 48
    alignment _ = 8
    peek p = CsX86Op
        <$> do
            t <- fromIntegral <$> {#get cs_x86_op->type#} p
            let bP = plusPtr p 8
            case toEnum t of
              X86OpReg -> (Reg . toEnum . fromIntegral) <$>
                  (peek bP :: IO CInt)
              X86OpImm -> Imm <$> peek bP
              X86OpFp -> (Fp . realToFrac) <$> (peek bP :: IO CDouble)
              X86OpMem -> Mem <$> peek bP
              _ -> return Undefined
        <*> (peekByteOff p 32) -- size
        <*> ((toEnum . fromIntegral) <$>
                (peekByteOff p 36 :: IO CInt)) -- avx_bcast
        <*> (toBool <$> (peekByteOff p 40 :: IO Word8)) -- avx_zero_opmask
    poke p (CsX86Op val s ab az) = do
        let bP = plusPtr p 8
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
        pokeByteOff p 32 s -- size
        pokeByteOff p 36 (fromIntegral $ fromEnum ab :: CInt) -- avx_bcast
        pokeByteOff p 40 (fromBool az :: Word8) -- avx_zero_opmask

-- instructions
data CsX86 = CsX86
    { prefix :: (Maybe Word8, Maybe Word8, Maybe Word8, Maybe Word8)
    , opcode :: [Word8]
    , rex :: Word8
    , addrSize :: Word8
    , modRM :: Word8
    , sib :: Maybe Word8
    , disp :: Maybe Int32
    , sibIndex :: X86Reg
    , sibScale :: Int8
    , sibBase :: X86Reg
    , sseCc :: X86SseCc
    , avxCc :: X86AvxCc
    , avxSae :: Bool
    , avxRm :: X86AvxRm
    , operands :: [CsX86Op]
    } deriving (Show, Eq)

instance Storable CsX86 where
    sizeOf _ = 432
    alignment _ = 8
    peek p = CsX86
        <$> do let bP = plusPtr p {#offsetof cs_x86->prefix#}
               [p0, p1, p2, p3] <- peekArray 4 bP :: IO [Word8]
               return (fromZero p0, fromZero p1, fromZero p2, fromZero p3)
        <*> (dropWhileEnd (== 0) <$>
            peekArray 4 (plusPtr p {#offsetof cs_x86->opcode#}))
        <*> (fromIntegral <$> {#get cs_x86->rex#} p)
        <*> (fromIntegral <$> {#get cs_x86->addr_size#} p)
        <*> (fromIntegral <$> {#get cs_x86->modrm#} p)
        <*> ((fromZero . fromIntegral) <$> {#get cs_x86->sib#} p)
        <*> ((fromZero . fromIntegral) <$> {#get cs_x86->disp#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->sib_index#} p)
        <*> (fromIntegral <$> {#get cs_x86->sib_scale#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->sib_base#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->sse_cc#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->avx_cc#} p)
        <*> (toBool <$> (peekByteOff p 36 :: IO Word8)) -- avx_sae
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->avx_rm#} p)
        <*> do num <- fromIntegral <$> {#get cs_x86->op_count#} p
               let ptr = plusPtr p {#offsetof cs_x86.operands#}
               peekArray num ptr
    poke p (CsX86 (p0, p1, p2, p3) op r a m s d sI sS sB sC aC aS aR o) = do
        let p' = [ fromMaybe 0 p0
                 , fromMaybe 0 p1
                 , fromMaybe 0 p2
                 , fromMaybe 0 p3
                 ]
            op' = op ++ replicate (4 - length op) 0
        pokeArray (plusPtr p {#offsetof cs_x86->prefix#}) p'
        pokeArray (plusPtr p {#offsetof cs_x86->opcode#}) op'
        {#set cs_x86->rex#} p (fromIntegral r)
        {#set cs_x86->addr_size#} p (fromIntegral a)
        {#set cs_x86->modrm#} p (fromIntegral m)
        {#set cs_x86->sib#} p (fromIntegral $ fromMaybe 0 s)
        {#set cs_x86->disp#} p (fromIntegral $ fromMaybe 0 d)
        {#set cs_x86->sib_index#} p (fromIntegral $ fromEnum sI)
        {#set cs_x86->sib_scale#} p (fromIntegral sS)
        {#set cs_x86->sib_base#} p (fromIntegral $ fromEnum sB)
        {#set cs_x86->sse_cc#} p (fromIntegral $ fromEnum sC)
        {#set cs_x86->avx_cc#} p (fromIntegral $ fromEnum aC)
        pokeByteOff p 36 (fromBool aS :: Word8) -- avx_sae
        {#set cs_x86->avx_rm#} p (fromIntegral $ fromEnum aR)
        {#set cs_x86->op_count#} p (fromIntegral $ length o)
        if length o > 8
           then error "operands overflew 8 elements"
           else pokeArray (plusPtr p {#offsetof cs_x86->operands#}) o

-- more enumerations
{#enum x86_insn as X86Insn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum x86_insn_group as X86InsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
