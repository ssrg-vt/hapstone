{-# LANGUAGE RecordWildCards #-}
module Hapstone.Capstone 
    ( disasmIO
    , disasmSimpleIO
    , Disassembler(..)
    , defaultSkipdataStruct
    , defaultAction
    ) where

import Data.Word

import Foreign.Ptr

import Hapstone.Internal.Capstone

-- | default setup for skipdata
defaultSkipdataStruct :: CsSkipdataStruct
defaultSkipdataStruct = CsSkipdataStruct ".db" nullFunPtr nullPtr

-- | default action to run on each instruction
defaultAction :: Csh -> CsInsn -> IO ()
defaultAction _ _ = return ()

-- | a structure holding settings for a disassembling action
data Disassembler a = Disassembler
    { arch :: CsArch -- ^ architecture
    , modes :: [CsMode] -- ^ disassembling modes
    , buffer :: [Word8] -- ^ buffer to disassemble
    , addr :: Word64 -- ^ address of first byte in the buffer
    , num :: Int -- ^ number of instructions to disassemble (0 for maximum)
    , detail :: Bool -- ^ include detailed information?
    , skip :: Maybe CsSkipdataStruct -- ^ setup SKIPDATA options
    , action :: Csh -> CsInsn -> IO a -- ^ action to run on each instruction
    }

-- | run a disassembler, throwing away the results of the custom action
disasmSimpleIO :: Disassembler a -> IO (Either CsErr [CsInsn])
disasmSimpleIO = fmap (fmap (map fst)) . disasmIO

-- | run a Disassembler
disasmIO :: Disassembler a -> IO (Either CsErr [(CsInsn, a)])
disasmIO d@Disassembler{..} = do (err, handle) <- csOpen arch modes
                                 res <- case err of
                                          CsErrOk -> disasmIOWithHandle handle
                                          _ -> return $ Left err
                                 csClose handle
                                 return res
    where disasmIOWithHandle handle = do
              err <- if detail
                        then csOption handle CsOptDetail CsOptOn
                        else return CsErrOk
              case err of
                CsErrOk -> disasmIOWithHandleDetail handle
                _ -> return $ Left err
          disasmIOWithHandleDetail handle = do
              err <- csSetSkipdata handle skip
              case err of
                CsErrOk -> do insns <- csDisasm arch handle buffer addr num
                              as <- mapM (action handle) insns
                              return . Right $ zip insns as
                _ -> return $ Left err
