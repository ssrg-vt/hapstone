module Hapstone.Internal.Util where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import System.IO.Unsafe (unsafePerformIO)

-- peeking with number conversion
peekNum :: (Integral a, Num b, Storable a) => Ptr a -> IO b
peekNum a = fromIntegral <$> peek a

-- number conversion and enum casting
getCULongFromEnum :: Enum e => e -> CULong
getCULongFromEnum = fromIntegral . fromEnum

-- cast a pointer before using with
withCast :: Storable a => a -> (Ptr b -> IO c) -> IO c
withCast a f = with a (f . castPtr)

-- bitwise OR a list of symbolic constants from an enumeration
combine :: (Enum e, Num n, Bits n) => [e] -> n
combine = foldr ((.|.) <$> fromIntegral . fromEnum) 0

-- lookup a possibly NULL-valued char *
stringLookup :: CString -> Maybe String
stringLookup s
    | s == nullPtr = Nothing
    | otherwise = Just . unsafePerformIO $ peekCString s

-- convert between Maybe and zero/nonzero Num
fromZero :: (Eq a, Num a) => a -> Maybe a
fromZero 0 = Nothing
fromZero v = Just v

-- peek if given pointer is not NULL
peekFunMaybe :: Storable a => (Ptr a -> IO a) -> Ptr a -> IO (Maybe a)
peekFunMaybe peekFun ptr
    | ptr == nullPtr = return Nothing
    | otherwise = Just <$> peekFun ptr
