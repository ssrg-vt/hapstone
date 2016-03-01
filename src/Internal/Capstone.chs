{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.Capstone where

{- TODO's:
* document all functions properly
* decide on a consistent use of pure and impure functions
  * do we want to make everything pure? no, we can't
  * do we want to make everything IO'ish then? or just where appropriate?
* split in multiple files
  * utils
  * data declarations
  * functions
* look at all array marshalling code and rewrite if incorrect
* add safeguards against overflows as well
-}

#include <capstone/capstone.h>

{#context lib = "capstone"#}

import Control.Monad (join)

import Foreign
import Foreign.C.Types
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr

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

-- we will skip user defined dynamic memory routines for now

-- options are, interestingly, represented by different types
{#enum cs_opt_type as CsOption {underscoreToCase} deriving (Show)#}
{#enum cs_opt_value as CsOptionState {underscoreToCase} deriving (Show)#}

-- operands
{#enum cs_op_type as CsOperand {underscoreToCase} deriving (Show)#}

-- instruction groups
{#enum cs_group_type as CsGroup {underscoreToCase} deriving (Show)#}

-- what is this SKIPDATA business whose callback function we happily omitted?

-- instruction information
-- TODO: use high-level types
data CsDetail = CsDetail
    { regsRead :: [CUChar]
    , regsWrite :: [CUChar]
    , groups :: [CUChar]
    , archInfo :: () -- TODO: implement
    } deriving Show

instance Storable CsDetail where
    sizeOf _ = {#sizeof cs_detail#}
    alignment _ = {#alignof cs_detail#}
    peek p = CsDetail <$> getRegsRead p <*>
        getRegsWrite p <*> getGroups p <*> pure ()
        -- TODO: read arch info
    poke p (CsDetail rR rW g _) = do
        pokeArray (plusPtr p {#offsetof cs_detail.regs_read#}) rR
        {#set cs_detail->regs_read_count#} p (fromIntegral $ length rR)
        pokeArray (plusPtr p {#offsetof cs_detail.regs_write#}) rW
        {#set cs_detail->regs_write_count#} p (fromIntegral $ length rW)
        pokeArray (plusPtr p {#offsetof cs_detail.groups#}) g
        {#set cs_detail->groups_count#} p (fromIntegral $ length g)
        -- TODO: write arch info

-- TODO: check whether this code is correct at all
-- (the get hooks seem out of place)
getRegsRead :: Ptr CsDetail -> IO [CUChar]
getRegsRead p = join $
    peekArray <$> (fromIntegral <$> {#get cs_detail->regs_read_count#} p)
              <*> {#get cs_detail->regs_read#} p

-- TODO: check whether this code is correct at all
-- (the get hooks seem out of place)
getRegsWrite :: Ptr CsDetail -> IO [CUChar]
getRegsWrite p = join $
    peekArray <$> (fromIntegral <$> {#get cs_detail->regs_write_count#} p)
              <*> {#get cs_detail->regs_read#} p

-- TODO: check whether this code is correct at all
-- (the get hooks seem out of place)
getGroups :: Ptr CsDetail -> IO [CUChar]
getGroups p = join $
    peekArray <$> (fromIntegral <$> {#get cs_detail->groups_count#} p)
              <*> {#get cs_detail->groups#} p

-- TODO: high level types
data CsInsn = CsInsn
    { insnId :: CUInt
    , address :: CULong
    , bytes :: [CUChar]
    , mnemonic :: String
    , opStr :: String
    , detail :: CsDetail
    } deriving Show

instance Storable CsInsn where
    sizeOf _ = {#sizeof cs_insn#}
    alignment _ = {#alignof cs_insn#}
    peek p = CsInsn <$> {#get cs_insn->id#} p
                    <*> {#get cs_insn->address#} p
                    <*> join (peekArray <$>
                          (fromIntegral <$> {#get cs_insn->size#} p)
                                        <*> {#get cs_insn->bytes#} p)
                    <*> (peekCString =<< {#get cs_insn->mnemonic#} p)
                    <*> (peekCString =<< {#get cs_insn->op_str#} p)
                    <*> (castPtr <$> {#get cs_insn->detail#} p >>= peek)
    poke p (CsInsn i a b m o d) = do
        {#set cs_insn->id#} p (fromIntegral i)
        {#set cs_insn->address#} p (fromIntegral a)
        {#set cs_insn->size#} p (fromIntegral $ length b)
        pokeArray (plusPtr p {#offsetof cs_insn.bytes#}) b
        withCString m ({#set cs_insn->mnemonic#} p)
        withCString o ({#set cs_insn->op_str#} p)
        poke (plusPtr p {#offsetof cs_insn.detail#}) d

-- TODO: we need to port the CS_INSN_OFFSET macro somehow I assume...
-- inlined C should be sufficient

{#enum cs_err as CsErr {underscoreToCase} deriving (Show)#}

{#fun pure cs_version as ^
    {alloca- `Int' peek'*, alloca- `Int' peek'*} -> `Int'#}

-- TODO: helper file
peek' :: (Integral a, Num b, Storable a) => Ptr a -> IO b
peek' a = fromIntegral <$> peek a

foreign import ccall "capstone/capstone.h cs_support"
    csSupport' :: CInt -> Bool

csSupport :: Enum a => a -> Bool
csSupport = csSupport' . fromIntegral . fromEnum

-- TODO: maybe make this pure and make it Csh with some marshalling everywhere?
{#fun cs_open as ^
    {`CsArch', `CsMode', alloca- `Csh' peek*} -> `CsErr'#}

{#fun cs_close as ^ {id `Ptr Csh'} -> `CsErr'#}

{#fun cs_option as ^ `Enum a' =>
    {`Csh', `CsOption', getCULongFromEnum `a'} -> `CsErr'#}

-- TODO: helper file
getCULongFromEnum :: Enum e => e -> CULong
getCULongFromEnum = fromIntegral . fromEnum

{#fun cs_errno as ^ {`Csh'} -> `CsErr'#}

{#fun cs_strerror as ^ {`CsErr'} -> `String'#}

foreign import ccall "capstone/capstone.h cs_disasm"
    csDisasm' :: Csh -- ^ handle
              -> Ptr CUChar -> CSize -- ^ buffer to disassemble
              -> CULong -- ^ address to start at
              -> CSize -- ^ number of instructins to disassemble
              -> Ptr (Ptr CsInsn) -- ^ where to put the instructions
              -> IO CSize -- ^ number of succesfully disassembled instructions

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

{#fun cs_free as ^ {castPtr `Ptr CsInsn', `Int'} -> `()'#}
{#fun cs_malloc as ^ {`Csh'} -> `Ptr CsInsn' castPtr#}
-- TODO: decide whether we want cs_disasm_iter

-- TODO: change these types to the appropriate enum types
{#fun pure cs_reg_name as ^ {`Csh', `Int'} -> `String'#}
{#fun pure cs_insn_name as ^ {`Csh', `Int'} -> `String'#}
{#fun pure cs_group_name as ^ {`Csh', `Int'} -> `String'#}

-- TODO: change these types to the appropriate enum types
-- TODO: use maybe types where applicable (in a higher-level API)
{#fun pure cs_insn_group as ^
    {`Csh', withCast* `CsInsn', `Int'} -> `Bool'#}
{#fun pure cs_reg_read as ^
    {`Csh', withCast* `CsInsn', `Int'} -> `Bool'#}
{#fun pure cs_reg_write as ^
    {`Csh', withCast* `CsInsn', `Int'} -> `Bool'#}
{#fun pure cs_op_count as ^
    {`Csh', withCast* `CsInsn', `Int'} -> `Int'#}
{#fun pure cs_op_index as ^
    {`Csh', withCast* `CsInsn', `Int', `Int'} -> `Int'#}

-- TODO: helper file
withCast :: Storable a => a -> (Ptr b -> IO c) -> IO c
withCast a f = with a (f . castPtr)
