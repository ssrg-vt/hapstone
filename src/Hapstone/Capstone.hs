{-# LANGUAGE RecordWildCards #-}
module Hapstone.Capstone where

import Data.Word

import Foreign.Ptr

import Hapstone.Internal.Capstone

import System.IO.Unsafe (unsafePerformIO)

-- default setup for skipdata
defaultSkipdataStruct :: CsSkipdataStruct
defaultSkipdataStruct = CsSkipdataStruct ".db" nullFunPtr nullPtr

data Disassembler a = Disassembler
    { arch :: CsArch
    , modes :: [CsMode]
    , buffer :: [Word8]
    , addr :: Word64
    , num :: Int
    , detail :: Bool
    , skip :: Maybe CsSkipdataStruct
    , action :: Csh -> CsInsn -> IO a
    }

disasmIO :: Disassembler a -> IO (Either CsErr [CsInsn])
disasmIO Disassembler{..} = undefined
