module Hapstone.Internal.Util where

import Foreign
import Foreign.C.Types
import Foreign.Ptr

peekNum :: (Integral a, Num b, Storable a) => Ptr a -> IO b
peekNum a = fromIntegral <$> peek a

getCULongFromEnum :: Enum e => e -> CULong
getCULongFromEnum = fromIntegral . fromEnum

withCast :: Storable a => a -> (Ptr b -> IO c) -> IO c
withCast a f = with a (f . castPtr)
