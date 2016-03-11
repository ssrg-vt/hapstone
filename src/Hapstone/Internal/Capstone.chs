{-# LANGUAGE ForeignFunctionInterface #-}
module Hapstone.Internal.Capstone where

{- TODO's:
* document all functions properly
* decide on a consistent use of pure and impure functions
  * do we want to make everything pure? no, we can't
  * do we want to make everything IO'ish then? or just where appropriate?
-}

#include <capstone/capstone.h>

{#context lib = "capstone"#}

import Control.Monad (join)

import Foreign
import Foreign.C.Types
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr

import Hapstone.Internal.Util

import System.IO.Unsafe (unsafePerformIO)

-- capstone's weird handle type
type Csh = CSize
{#typedef csh Csh#}

-- supported architectures
{#enum cs_arch as CsArch {underscoreToCase} deriving (Show)#}

-- support constants
{#enum define CsSupport
    { CS_SUPPORT_DIET as CsSupportDiet
    , CS_SUPPORT_X86_REDUCE as CsSupportX86Reduce} deriving (Show)#}

-- work modes
{#enum cs_mode as CsMode {underscoreToCase} deriving (Show)#}

-- TODO: we will skip user defined dynamic memory routines for now

-- options are, interestingly, represented by different types
{#enum cs_opt_type as CsOption {underscoreToCase} deriving (Show)#}
{#enum cs_opt_value as CsOptionState {underscoreToCase} deriving (Show)#}

-- arch-uniting operand type
{#enum cs_op_type as CsOperand {underscoreToCase} deriving (Show)#}

-- instruction groups
{#enum cs_group_type as CsGroup {underscoreToCase} deriving (Show)#}

-- TODO: what is this SKIPDATA business whose callback function
-- we happily omitted?

-- instruction information
data CsDetail = CsDetail
    { regsRead :: [Word8]
    , regsWrite :: [Word8]
    , groups :: [Word8]
    , archInfo :: () -- TODO: implement
    } deriving Show

instance Storable CsDetail where
    sizeOf _ = {#sizeof cs_detail#}
    alignment _ = {#alignof cs_detail#}
    peek p = CsDetail
        <$> do num <- fromIntegral <$> {#get cs_detail->regs_read_count#} p
               let ptr = plusPtr p {#offsetof cs_detail->regs_read#}
               peekArray num ptr
        <*> do num <- fromIntegral <$> {#get cs_detail->regs_write_count#} p
               let ptr = plusPtr p {#offsetof cs_detail->regs_write#}
               peekArray num ptr
        <*> do num <- fromIntegral <$> {#get cs_detail->groups_count#} p
               let ptr = plusPtr p {#offsetof cs_detail->groups#}
               peekArray num ptr
        <*> pure ()
        -- TODO: read arch info
    poke p (CsDetail rR rW g _) = do
        {#set cs_detail->regs_read_count#} p (fromIntegral $ length rR)
        if length rR > 12
           then error "regs_read overflew 12 bytes"
           else pokeArray (plusPtr p {#offsetof cs_detail.regs_read#}) rR
        {#set cs_detail->regs_write_count#} p (fromIntegral $ length rW)
        if length rW > 20
           then error "regs_write overflew 20 bytes"
           else pokeArray (plusPtr p {#offsetof cs_detail.regs_write#}) rW
        {#set cs_detail->groups_count#} p (fromIntegral $ length g)
        if length g > 8
           then error "groups overflew 8 bytes"
           else pokeArray (plusPtr p {#offsetof cs_detail.groups#}) g
        -- TODO: write arch info

-- instructions
data CsInsn = CsInsn
    { insnId :: Word32
    , address :: Word64
    , bytes :: [Word8]
    , mnemonic :: String
    , opStr :: String
    , detail :: CsDetail
    } deriving Show

instance Storable CsInsn where
    sizeOf _ = {#sizeof cs_insn#}
    alignment _ = {#alignof cs_insn#}
    peek p = CsInsn
        <$> (fromIntegral <$> {#get cs_insn->id#} p)
        <*> (fromIntegral <$> {#get cs_insn->address#} p)
        <*> do num <- fromIntegral <$> {#get cs_insn->size#} p
               let ptr = plusPtr p {#offsetof cs_insn->bytes#}
               peekArray num ptr
        <*> (peekCString =<< {#get cs_insn->mnemonic#} p)
        <*> (peekCString =<< {#get cs_insn->op_str#} p)
        <*> (castPtr <$> {#get cs_insn->detail#} p >>= peek)
    poke p (CsInsn i a b m o d) = do
        {#set cs_insn->id#} p (fromIntegral i)
        {#set cs_insn->address#} p (fromIntegral a)
        {#set cs_insn->size#} p (fromIntegral $ length b)
        if length b > 16
           then error "bytes overflew 16 bytes"
           else pokeArray (plusPtr p {#offsetof cs_insn.bytes#}) b
        if length m >= 32
           then error "mnemonic overflew 32 bytes"
           else withCString m ({#set cs_insn->mnemonic#} p)
        if length o >= 160
           then error "op_str overflew 160 bytes"
           else withCString o ({#set cs_insn->op_str#} p)
        csDetailPtr <- castPtr <$> ({#get cs_insn->detail#} p)
        poke csDetailPtr d

-- our own port of the CS_INSN_OFFSET macro
csInsnOffset :: Ptr CsInsn -> Int -> Int
csInsnOffset p n = unsafePerformIO $
    (-) <$> getAddr (plusPtr p (n * {#sizeof cs_insn#})) <*> getAddr p
    where getAddr p = fromIntegral <$> {#get cs_insn->address#} p

-- possible error conditions
{#enum cs_err as CsErr {underscoreToCase} deriving (Show)#}

-- get the library version
{#fun pure cs_version as ^
    {alloca- `Int' peekNum*, alloca- `Int' peekNum*} -> `Int'#}

-- get information on supported features
foreign import ccall "capstone/capstone.h cs_support"
    csSupport' :: CInt -> Bool
csSupport :: Enum a => a -> Bool
csSupport = csSupport' . fromIntegral . fromEnum

-- open a new disassembly handle
{#fun cs_open as ^
    {`CsArch', combine `[CsMode]', alloca- `Csh' peek*} -> `CsErr'#}

-- close a handle obtained by cs_open/csOpen
{#fun cs_close as ^ {id `Ptr Csh'} -> `CsErr'#}

-- set an option on a handle
{#fun cs_option as ^ `Enum a' =>
    {`Csh', `CsOption', getCULongFromEnum `a'} -> `CsErr'#}

-- get the last error from a handle
{#fun cs_errno as ^ {`Csh'} -> `CsErr'#}

-- get the description of an error
{#fun cs_strerror as ^ {`CsErr'} -> `String'#}

-- disassemble a buffer
foreign import ccall "capstone/capstone.h cs_disasm"
    csDisasm' :: Csh -- handle
              -> Ptr CUChar -> CSize -- buffer to disassemble
              -> CULong -- address to start at
              -> CSize -- number of instructins to disassemble
              -> Ptr (Ptr CsInsn) -- where to put the instructions
              -> IO CSize -- number of succesfully disassembled instructions

csDisasm :: Csh -> [Word8] -> Word64 -> Int -> IO [CsInsn]
csDisasm handle bytes addr num = do
    array <- newArray $ map fromIntegral bytes
    passedPtr <- malloc :: IO (Ptr (Ptr CsInsn))
    resNum <- fromIntegral <$> csDisasm' handle array
        (fromIntegral $ length bytes) (fromIntegral addr)
        (fromIntegral num) passedPtr
    resPtr <- peek passedPtr
    free passedPtr
    res <- mapM (peek . plusPtr resPtr . ({#sizeof cs_insn#} *)) [0..resNum]
    csFree resPtr resNum
    return res

-- free an instruction struct
{#fun cs_free as ^ {castPtr `Ptr CsInsn', `Int'} -> `()'#}

-- allocate space for an instruction struct
{#fun cs_malloc as ^ {`Csh'} -> `Ptr CsInsn' castPtr#}

-- TODO: decide whether we want cs_disasm_iter

-- get a register's name as a String
{#fun pure cs_reg_name as csRegName' {`Csh', `Int'} -> `CString'#}
csRegName :: Enum e => Csh -> e -> Maybe String
csRegName h = stringLookup . csRegName' h . fromEnum

-- get a instruction's name as a String
{#fun pure cs_insn_name as csInsnName' {`Csh', `Int'} -> `CString'#}
csInsnName :: Enum e => Csh -> e -> Maybe String
csInsnName h = stringLookup . csInsnName' h . fromEnum

-- get a instruction group's name as a String
{#fun pure cs_group_name as csGroupName' {`Csh', `Int'} -> `CString'#}
csGroupName :: Enum e => Csh -> e -> Maybe String
csGroupName h = stringLookup . csGroupName' h . fromEnum

-- and change these types to the appropriate enum types
-- TODO fix with ghci :/
{-{#fun pure cs_insn_group as ^
    {`Csh', withCast* `CsInsn', `Int'} -> `Bool'#}
{#fun pure cs_reg_read as ^
    {`Csh', withCast* `CsInsn', `Int'} -> `Bool'#}
{#fun pure cs_reg_write as ^
    {`Csh', withCast* `CsInsn', `Int'} -> `Bool'#}-}
{#fun pure cs_op_count as ^
    {`Csh', withCast* `CsInsn', `Int'} -> `Int'#}
{#fun pure cs_op_index as ^
    {`Csh', withCast* `CsInsn', `Int', `Int'} -> `Int'#}
