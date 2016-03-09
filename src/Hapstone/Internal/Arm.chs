{-# LANGUAGE ForeignFunctionInterface #-}
module Hapstone.Internal.Arm where

#include <capstone/arm.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

{#enum arm_shifter as ArmShifter {underscoreToCase} deriving (Show)#}
{#enum arm_cc as ArmConditionCode {underscoreToCase} deriving (Show)#}
{#enum arm_sysreg as ArmSysreg {underscoreToCase} deriving (Show)#}
{#enum arm_mem_barrier as ArmMemBarrier {underscoreToCase} deriving (Show)#}

{#enum arm_op_type as ArmOpType {underscoreToCase} deriving (Show)#}

{#enum arm_setend_type as ArmSetendType {underscoreToCase} deriving (Show)#}
{#enum arm_cpsmode_type as ArmCpsmodeType {underscoreToCase} deriving (Show)#}
{#enum arm_cpsflag_type as ArmCpsflagType {underscoreToCase} deriving (Show)#}
{#enum arm_vectordata_type as
    ArmVectordataType {underscoreToCase} deriving (Show)#}

data ArmOpMemStruct = ArmOpMemStruct Word32 Word32 Int32 Int32

instance Storable ArmOpMemStruct where
    sizeOf _ = {#sizeof arm_op_mem#}
    alignment _ = {#alignof arm_op_mem#}
    peek p = ArmOpMemStruct
        <$> (fromIntegral <$> {#get arm_op_mem->base#} p)
        <*> (fromIntegral <$> {#get arm_op_mem->index#} p)
        <*> (fromIntegral <$> {#get arm_op_mem->scale#} p)
        <*> (fromIntegral <$> {#get arm_op_mem->disp#} p)
    poke p (ArmOpMemStruct b i s d) = do
        {#set arm_op_mem->base#} p (fromIntegral b)
        {#set arm_op_mem->index#} p (fromIntegral i)
        {#set arm_op_mem->scale#} p (fromIntegral s)
        {#set arm_op_mem->disp#} p (fromIntegral d)

data CsArmOpValue
    = Reg Word32
    | Sysreg Word32
    | Imm Int32
    | Cimm Int32
    | Pimm Int32
    | Fp Double
    | Mem ArmOpMemStruct
    | Setend ArmSetendType
    | Undefined

data CsArmOp = CsArmOp
    { vectorIndex :: Int32
    , shift :: (ArmShifter, Word32)
    , value :: CsArmOpValue
    , subtracted :: Bool
    }

instance Storable CsArmOp where
    sizeOf _ = {#sizeof cs_arm_op#}
    alignment _ = {#alignof cs_arm_op#}
    peek p = CsArmOp
        <$> (fromIntegral <$> {#get cs_arm_op->vector_index#} p)
        <*> ((,) <$>
            ((toEnum . fromIntegral) <$> {#get cs_arm_op->shift.type#} p) <*>
            (fromIntegral <$> {#get cs_arm_op->shift.value#} p))
        <*> do
            t <- fromIntegral <$> {#get cs_arm_op->type#} p :: IO Int
            let bP = plusPtr p -- FIXME: maybe alignment will bite us!
                   ({#offsetof cs_arm_op.type#} + {#sizeof arm_op_type#})
            case toEnum t of
              ArmOpReg -> (Reg . fromIntegral) <$> (peek bP :: IO CUInt)
              ArmOpSysreg -> (Sysreg . fromIntegral) <$> (peek bP :: IO CUInt)
              ArmOpImm -> (Imm . fromIntegral) <$> (peek bP :: IO CInt)
              ArmOpCimm -> (Cimm . fromIntegral) <$> (peek bP :: IO CInt)
              ArmOpPimm -> (Pimm . fromIntegral) <$> (peek bP :: IO CInt)
              ArmOpFp -> (Fp . realToFrac) <$> (peek bP :: IO CDouble)
              ArmOpMem -> Mem <$> (peek bP :: IO ArmOpMemStruct)
              ArmOpSetend -> (Setend . toEnum . fromIntegral) <$>
                  (peek bP :: IO CInt)
              _ -> return Undefined
        <*> ({#get cs_arm_op->subtracted#} p)
    poke p (CsArmOp vI (sh, shV) val sub) = do
        {#set cs_arm_op->vector_index#} p (fromIntegral vI)
        {#set cs_arm_op->shift.type#} p (fromIntegral $ fromEnum sh)
        {#set cs_arm_op->shift.value#} p (fromIntegral shV)
        let bP = plusPtr p -- FIXME: maybe alignment will bite us!
               ({#offsetof cs_arm_op.type#} + {#sizeof arm_op_type#})
            setType = {#set cs_arm_op->type#} p . fromIntegral . fromEnum
        case val of
          Reg r -> do
              poke bP (fromIntegral r :: CUInt)
              setType ArmOpReg
          Sysreg r -> do
              poke bP (fromIntegral r :: CUInt)
              setType ArmOpSysreg
          Imm i -> do
              poke bP (fromIntegral i :: CInt)
              setType ArmOpImm
          Cimm i -> do
              poke bP (fromIntegral i :: CInt)
              setType ArmOpCimm
          Pimm i -> do
              poke bP (fromIntegral i :: CInt)
              setType ArmOpPimm
          Fp f -> do
              poke bP (realToFrac f :: CDouble)
              setType ArmOpFp
          Mem m -> do
              poke bP m
              setType ArmOpMem
          Setend s -> do
              poke bP (fromIntegral $ fromEnum s :: CInt)
              setType ArmOpSetend
          _ -> setType ArmOpInvalid
        {#set cs_arm_op->subtracted#} p sub

data CsArm = CsArm
    { usermode :: Bool
    , vectorSize :: Int32
    , vectorData :: ArmVectordataType
    , cpsMode :: ArmCpsmodeType
    , cpsFlag :: ArmCpsflagType
    , cc :: ArmConditionCode
    , updateFlags :: Bool
    , writeback :: Bool
    , memBarrier :: ArmMemBarrier
    , operands :: [CsArmOp]
    }

instance Storable CsArm where
    sizeOf _ = {#sizeof cs_arm#}
    alignment _ = {#alignof cs_arm#}
    peek p = CsArm
        <$> ({#get cs_arm->usermode#} p)
        <*> (fromIntegral <$> {#get cs_arm->vector_size#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->vector_data#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->cps_mode#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->cps_flag#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->cc#} p)
        <*> ({#get cs_arm->update_flags#} p)
        <*> ({#get cs_arm->writeback#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->mem_barrier#} p)
        <*> do num <- fromIntegral <$> {#get cs_arm->op_count#} p
               let ptr = plusPtr p {#offsetof cs_arm.operands#}
               peekArray num ptr
    poke p (CsArm u vS vD cM cF cc uF w m o) = do
        {#set cs_arm->usermode#} p u
        {#set cs_arm->vector_size#} p (fromIntegral vS)
        {#set cs_arm->vector_data#} p (fromIntegral $ fromEnum vD)
        {#set cs_arm->cps_mode#} p (fromIntegral $ fromEnum cM)
        {#set cs_arm->cps_flag#} p (fromIntegral $ fromEnum cF)
        {#set cs_arm->cc#} p (fromIntegral $ fromEnum cc)
        {#set cs_arm->update_flags#} p uF
        {#set cs_arm->writeback#} p w
        {#set cs_arm->mem_barrier#} p (fromIntegral $ fromEnum m)
        {#set cs_arm->op_count#} p (fromIntegral $ length o)
        if length o > 36
           then error "operands overflew 36 elements"
           else pokeArray (plusPtr p {#offsetof cs_arm->operands#}) o

{#enum arm_reg as ArmReg {underscoreToCase} deriving (Show)#}
{#enum arm_insn as ArmInsn {underscoreToCase} deriving (Show)#}
{#enum arm_insn_group as ArmInsnGroup {underscoreToCase} deriving (Show)#}
