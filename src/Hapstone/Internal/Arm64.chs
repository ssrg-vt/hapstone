{-# LANGUAGE ForeignFunctionInterface #-}
module Hapstone.Internal.Arm64 where

#include <capstone/arm64.h>

{#context lib = "capstone"#}

import Control.Monad (join)

import Foreign
import Foreign.C.Types

-- enumarations
{#enum arm64_shifter as Arm64Shifter {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum arm64_extender as Arm64Extender {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum arm64_cc as Arm64ConditionCode {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum arm64_sysreg as Arm64Sysreg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum arm64_msr_reg as Arm64MsrReg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum arm64_pstate as Arm64Pstate {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum arm64_vas as Arm64Vas {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum arm64_vess as Arm64Vess {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum arm64_barrier_op as Arm64BarrierOp {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum arm64_op_type as Arm64OpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum arm64_tlbi_op as Arm64TlbiOp {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum arm64_at_op as Arm64AtOp {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum arm64_dc_op as Arm64DcOp {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum arm64_ic_op as Arm64IcOp {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum arm64_prefetch_op as Arm64PrefetchOp {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- memory access operands
data Arm64OpMemStruct = Arm64OpMemStruct Word32 Word32 Int32
    deriving (Show, Eq)

instance Storable Arm64OpMemStruct where
    sizeOf _ = {#sizeof arm64_op_mem#}
    alignment _ = {#alignof arm64_op_mem#}
    peek p = Arm64OpMemStruct
        <$> (fromIntegral <$> {#get arm64_op_mem->base#} p)
        <*> (fromIntegral <$> {#get arm64_op_mem->index#} p)
        <*> (fromIntegral <$> {#get arm64_op_mem->disp#} p)
    poke p (Arm64OpMemStruct b i d) = do
        {#set arm64_op_mem->base#} p (fromIntegral b)
        {#set arm64_op_mem->index#} p (fromIntegral i)
        {#set arm64_op_mem->disp#} p (fromIntegral d)

-- possible operand types
data CsArm64OpValue
    = Reg Word32
    | Imm Int64
    | CImm Int64
    | Fp Double
    | Mem Arm64OpMemStruct
    | Pstate Arm64Pstate
    | Sys Word32
    | Prefetch Arm64PrefetchOp
    | Barrier Arm64BarrierOp
    | Undefined
    deriving (Show, Eq)

-- operands
data CsArm64Op = CsArm64Op
    { vectorIndex :: Int32
    , vas :: Arm64Vas
    , vess :: Arm64Vess
    , shift :: (Arm64Shifter, Word32)
    , ext :: Arm64Extender
    , value :: CsArm64OpValue
    } deriving (Show, Eq)

instance Storable CsArm64Op where
    sizeOf _ = 48
    alignment _ = 8
    peek p = CsArm64Op
        <$> (fromIntegral <$> {#get cs_arm64_op->vector_index#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm64_op->vas#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm64_op->vess#} p)
        <*> ((,) <$>
            ((toEnum . fromIntegral) <$> {#get cs_arm64_op->shift.type#} p) <*>
            (fromIntegral <$> {#get cs_arm64_op->shift.value#} p))
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm64_op->ext#} p)
        <*> do
            t <- fromIntegral <$> {#get cs_arm64_op->type#} p
            let bP = plusPtr p 32
            case toEnum t of
              Arm64OpReg -> (Reg . fromIntegral) <$> (peek bP :: IO CUInt)
              Arm64OpImm -> (Imm . fromIntegral) <$> (peek bP :: IO Int64)
              Arm64OpCimm -> (CImm . fromIntegral) <$> (peek bP :: IO Int64)
              Arm64OpFp -> (Fp . realToFrac) <$> (peek bP :: IO CDouble)
              Arm64OpMem -> Mem <$> peek bP
              Arm64OpRegMsr -> (Pstate . toEnum . fromIntegral) <$>
                 (peek bP :: IO CInt)
              Arm64OpSys -> (Sys . fromIntegral) <$> (peek bP :: IO CUInt)
              Arm64OpPrefetch -> (Prefetch . toEnum . fromIntegral) <$>
                 (peek bP :: IO CInt)
              Arm64OpBarrier -> (Barrier . toEnum . fromIntegral) <$>
                 (peek bP :: IO CInt)
              _ -> return Undefined
    poke p (CsArm64Op vI va ve (sh, shV) ext val) = do
        {#set cs_arm64_op->vector_index#} p (fromIntegral vI)
        {#set cs_arm64_op->vas#} p (fromIntegral $ fromEnum va)
        {#set cs_arm64_op->vess#} p (fromIntegral $ fromEnum ve)
        {#set cs_arm64_op->shift.type#} p (fromIntegral $ fromEnum sh)
        {#set cs_arm64_op->shift.value#} p (fromIntegral shV)
        {#set cs_arm64_op->ext#} p (fromIntegral $ fromEnum ext)
        let bP = plusPtr p 32
            setType = {#set cs_arm64_op->type#} p . fromIntegral . fromEnum
        case val of
          Reg r -> do
              poke bP (fromIntegral r :: CUInt)
              setType Arm64OpReg
          Imm i -> do
              poke bP (fromIntegral i :: Int64)
              setType Arm64OpImm
          CImm i -> do
              poke bP (fromIntegral i :: Int64)
              setType Arm64OpCimm
          Fp f -> do
              poke bP (realToFrac f :: CDouble)
              setType Arm64OpFp
          Mem m -> do
              poke bP m
              setType Arm64OpMem
          Pstate p -> do
              poke bP (fromIntegral $ fromEnum p :: CInt)
              setType Arm64OpRegMsr
          Sys s -> do
              poke bP (fromIntegral s :: CUInt)
              setType Arm64OpSys
          Prefetch p -> do
              poke bP (fromIntegral $ fromEnum p :: CInt)
              setType Arm64OpPrefetch
          Barrier b -> do
              poke bP (fromIntegral $ fromEnum b :: CInt)
              setType Arm64OpBarrier
          _ -> setType Arm64OpInvalid

-- instructions
data CsArm64 = CsArm64
    { cc :: Arm64ConditionCode
    , updateFlags :: Bool
    , writeback :: Bool
    , operands :: [CsArm64Op]
    } deriving (Show, Eq)

instance Storable CsArm64 where
    sizeOf _ = 392
    alignment _ = 12
    peek p = CsArm64
        <$> (toEnum . fromIntegral <$> {#get cs_arm64->cc#} p)
        <*> (toBool <$> (peekByteOff p 4 :: IO Word8)) -- update_flags
        <*> (toBool <$> (peekByteOff p 5 :: IO Word8)) -- writeback
        <*> do num <- fromIntegral <$> {#get cs_arm64->op_count#} p
               let ptr = plusPtr p {#offsetof cs_arm64.operands#}
               peekArray num ptr
    poke p (CsArm64 cc uF w o) = do
        {#set cs_arm64->cc#} p (fromIntegral $ fromEnum cc)
        pokeByteOff p 4 (fromBool uF :: Word8) -- update_flags
        pokeByteOff p 5 (fromBool w :: Word8) -- writeback
        {#set cs_arm64->op_count#} p (fromIntegral $ length o)
        if length o > 8
           then error "operands overflew 8 elements"
           else pokeArray (plusPtr p {#offsetof cs_arm64->operands#}) o

-- more enumerations
{#enum arm64_reg as Arm64Reg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum arm64_insn as Arm64Insn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum arm64_insn_group as Arm64InsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
