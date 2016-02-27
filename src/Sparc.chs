{-# LANGUAGE ForeignFunctionInterface #-}
module Sparc where

#include <capstone/sparc.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

{#enum sparc_cc as SparcCc {underscoreToCase} deriving (Show)#}
{#enum sparc_hint as SparcHint {underscoreToCase} deriving (Show)#}

{#enum sparc_op_type as SparcOpType {underscoreToCase} deriving (Show)#}

data SparcOpMemStruct = SparcOpMemStruct Word8 Word8 Int32

instance Storable SparcOpMemStruct where
    sizeOf _ = {#sizeof sparc_op_mem#}
    alignment _ = {#alignof sparc_op_mem#}
    peek p = SparcOpMemStruct <$> (fromIntegral <$> peek (basePtr p)) <*>
        (fromIntegral <$> peek (indexPtr p)) <*>
        (fromIntegral <$> peek (dispPtr p))
    poke p (SparcOpMemStruct b i d) = do
        poke (basePtr p) (fromIntegral b)
        poke (indexPtr p) (fromIntegral i)
        poke (dispPtr p) (fromIntegral d)

-- TODO: helper file
basePtr, indexPtr :: Ptr SparcOpMemStruct -> Ptr CUChar
basePtr p = plusPtr p {#offsetof sparc_op_mem.base#}
indexPtr p = plusPtr p {#offsetof sparc_op_mem.disp#}
dispPtr :: Ptr SparcOpMemStruct -> Ptr CInt
dispPtr p = plusPtr p {#offsetof sparc_op_mem.disp#}
