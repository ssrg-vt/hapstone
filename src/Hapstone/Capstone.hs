{-# LANGUAGE RecordWildCards, ForeignFunctionInterface, RankNTypes #-}
module Hapstone.Capstone 
    ( disasmIO
    , disasmSimpleIO
    , Disassembler(..)
    , defaultSkipdataStruct
    , defaultAction
    , mkCallback
    ) where

import Data.Word

import Foreign
import Foreign.C.Types
import Foreign.Ptr

import Hapstone.Internal.Capstone

-- | default setup for skipdata
defaultSkipdataStruct :: CsSkipdataStruct
defaultSkipdataStruct = CsSkipdataStruct ".db" nullFunPtr nullPtr

foreign import ccall "wrapper"
  allocCallback :: (Ptr Word8 -> CSize -> CSize -> Ptr () -> IO CSize)
                -> IO CsSkipdataCallback

-- | wrap a relatively safe function to get a callback
-- "safe" in this context means that the buffer remains unmodified
mkCallback :: Storable a => (Storable a => ([Word8], [Word8]) -> a -> IO CSize)
           -> IO CsSkipdataCallback
mkCallback = allocCallback . mkCallback'

mkCallback' :: Storable a
           => (Storable a => ([Word8], [Word8]) -> a -> IO CSize)
           -> Ptr Word8 -> CSize -> CSize -> Ptr () -> IO CSize
mkCallback' func ptr size off user_data = do
    buf <- splitAt (fromIntegral off) <$> peekArray (fromIntegral size) ptr
    arg <- peek (castPtr user_data)
    func buf arg

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
