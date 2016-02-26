{-# LANGUAGE ForeignFunctionInterface #-}
module Arm where

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

-- TODO: port cs_arm_op struct
-- TODO: port cs_arm struct

{#enum arm_reg as ArmReg {underscoreToCase} deriving (Show)#}
{#enum arm_insn as ArmInsn {underscoreToCase} deriving (Show)#}
{#enum arm_insn_group as ArmInsnGroup {underscoreToCase} deriving (Show)#}
