{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.Ppc
Description : PPC architecture header ported using C2HS + some boilerplate
Copyright   : (c) Inokentiy Babushkin, 2016
License     : BSD3
Maintainer  : Inokentiy Babushkin <inokentiy.babushkin@googlemail.com>
Stability   : experimental

This module contains PPC specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.Ppc where

#include <capstone/ppc.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

-- | PPC branch codes for some branch instructions
{#enum ppc_bc as PpcBc {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | PPC branch hint for some branch instructions
{#enum ppc_bh as PpcBh {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | operand type for instruction's operands
{#enum ppc_op_type as PpcOpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | PPC registers
{#enum ppc_reg as PpcReg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | memory access operands
-- associated with 'Ppc64OpMem' operand type
data PpcOpMemStruct = PpcOpMemStruct 
    { base :: PpcReg -- ^ base register
    , disp :: Int32 -- ^ displacement/offset value
    } deriving (Show, Eq)

instance Storable PpcOpMemStruct where
    sizeOf _ = {#sizeof ppc_op_mem#}
    alignment _ = {#alignof ppc_op_mem#}
    peek p = PpcOpMemStruct
        <$> ((toEnum . fromIntegral) <$> {#get ppc_op_mem->base#} p)
        <*> (fromIntegral <$> {#get ppc_op_mem->disp#} p)
    poke p (PpcOpMemStruct b d) = do
        {#set ppc_op_mem->base#} p (fromIntegral $ fromEnum b)
        {#set ppc_op_mem->disp#} p (fromIntegral d)

-- | CRX operands
-- associated with 'Ppc64OpCrx' operand type
data PpcOpCrxStruct = PpcOpCrxStruct
    { scale :: Word32
    , reg :: PpcReg
    , cond :: PpcBc
    } deriving (Show, Eq)

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
        {#set ppc_op_crx->cond#} p (fromIntegral $ fromEnum c)

-- | instruction operands
data CsPpcOp
    = Reg PpcReg -- ^ register value for 'PpcOpReg' operands
    | Imm Int32 -- ^ immediate value for 'PpcOpImm' operands
    | Mem PpcOpMemStruct -- ^ base/disp value for 'PpcOpMem' operands
    | Crx PpcOpCrxStruct -- ^ operand with condition register
    | Undefined -- ^ invalid operand value, for 'PpcOpInvalid' operand
    deriving (Show, Eq)

instance Storable CsPpcOp where
    sizeOf _ = 16
    alignment _ = 4
    peek p = do
        t <- fromIntegral <$> {#get cs_ppc_op->type#} p
        let bP = plusPtr p 4
        case toEnum t of
          PpcOpReg -> (Reg . toEnum . fromIntegral) <$> (peek bP :: IO CInt)
          PpcOpImm -> Imm <$> peek bP
          PpcOpMem -> Mem <$> peek bP
          PpcOpCrx -> Crx <$> peek bP
          _ -> return Undefined
    poke p op = do
        let bP = plusPtr p 4
            setType = {#set cs_ppc_op->type#} p . fromIntegral . fromEnum
        case op of
          Reg r -> do
              poke bP (fromIntegral $ fromEnum r :: CInt)
              setType PpcOpReg
          Imm i -> do
              poke bP i
              setType PpcOpImm
          Mem m -> do
              poke bP m
              setType PpcOpMem
          Crx c -> do
              poke bP c
              setType PpcOpCrx
          _ -> setType PpcOpInvalid

-- | instruction datatype
data CsPpc = CsPpc 
    { bc :: PpcBc -- ^ branch code for branch instructions
    , bh :: PpcBh -- ^ branch hint for branch instructions
    , updateCr0 :: Bool -- ^ does this instruction update CR0?
    , operands :: [CsPpcOp] -- ^ operand list of this instruction, *MUST*
                            -- have <= 8 elements, else you'll get a runtime
                            -- error when you (implicitly) try to write it to
                            -- write it to memory via it's Storable instance
    } deriving (Show, Eq)

instance Storable CsPpc where
    sizeOf _ = 140
    alignment _ = 4
    peek p = CsPpc
        <$> ((toEnum . fromIntegral) <$> {#get cs_ppc->bc#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_ppc->bh#} p)
        <*> (toBool <$> (peekByteOff p 8 :: IO Word8)) -- update_cr0
        <*> do num <- fromIntegral <$> {#get cs_ppc->op_count#} p
               let ptr = plusPtr p {#offsetof cs_ppc.operands#}
               peekArray num ptr
    poke p (CsPpc bc bh u o) = do
        {#set cs_ppc->bc#} p (fromIntegral $ fromEnum bc)
        {#set cs_ppc->bh#} p (fromIntegral $ fromEnum bh)
        pokeByteOff p 8 (fromBool u :: Word8) -- update_cr0
        {#set cs_ppc->op_count#} p (fromIntegral $ length o)
        if length o > 8
           then error "operands overflew 8 elements"
           else pokeArray (plusPtr p {#offsetof cs_ppc->operands#}) o

-- | PPC instructions
{#enum ppc_insn as PpcInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | PPC instruction groups
{#enum ppc_insn_group as PpcInsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
