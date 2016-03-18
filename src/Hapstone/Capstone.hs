module Hapstone.Capstone 
    ( defaultSkipdataStruct
    , disasmBufferFrame
    , disasmBuffer
    , disasmBufferSkipdata
    ) where

import Data.Word

import Foreign.Ptr

import Hapstone.Internal.Capstone

import System.IO.Unsafe (unsafePerformIO)

-- gracefully acquire a handle
getHandle :: CsArch -> [CsMode] -> IO (Either CsErr Csh)
getHandle arch modes = do
    (err, handle) <-csOpen arch modes
    case err of
      CsErrOk -> return $ Right handle
      _ -> return $ Left err

-- default setup for skipdata
defaultSkipdataStruct :: CsSkipdataStruct
defaultSkipdataStruct = CsSkipdataStruct ".db" nullFunPtr nullPtr

-- disassembler function type
type Disassembler = Csh
                  -> [Word8]
                  -> Word64
                  -> Int
                  -> IO (Either CsErr [CsInsn])

-- diassembly function, parametrized
disasmBufferFrame :: Disassembler -> CsArch -> [CsMode] 
                  -> [Word8] -> Word64 -> Int -> Either CsErr [CsInsn]
disasmBufferFrame disas arch modes buf addr num = unsafePerformIO $ do
    handle <- getHandle arch modes
    case handle of
      Right h -> disas h buf addr num <* csClose h
      Left e -> return $ Left e

-- wrap csDisasm to form a Disassembler
csDisasmWrap :: Disassembler
csDisasmWrap h b a n = Right <$> csDisasm h b a n

-- disassemble a buffer
disasmBuffer :: CsArch -> [CsMode] -> [Word8] -> Word64 -> Int
             -> Either CsErr [CsInsn]
disasmBuffer = disasmBufferFrame csDisasmWrap

-- disassemble a buffer providing a SKIPDATA structure
disasmBufferSkipdata :: CsSkipdataStruct -> CsArch -> [CsMode] -> [Word8]
                     -> Word64 -> Int -> Either CsErr [CsInsn]
disasmBufferSkipdata skip = disasmBufferFrame (disasSkipdata skip)
    where disasSkipdata s h b a n = do 
              err <- csSetSkipdata h (Just s)
              case err of
                CsErrOk -> csDisasmWrap h b a n
                e -> return $ Left e
