{-# LANGUAGE ForeignFunctionInterface #-}
module FFIHAXX (reopenStdin) where

import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Types (Fd)

stdin :: IO (Ptr Fd)
stdin = peek c_stdin

reopenStdin :: IO (Ptr Fd)
reopenStdin = stdin >>= freopen "/dev/tty" "r"

freopen :: String -> String -> Ptr Fd -> IO (Ptr Fd)
freopen fileName fileMode fdPtr = do
  fn <- newCString fileName
  fm <- newCString fileMode
  c_freopen fn fm fdPtr

foreign import ccall "&stdin" c_stdin :: Ptr (Ptr Fd)
foreign import ccall "freopen" c_freopen :: CString -> CString -> Ptr Fd -> IO (Ptr Fd)
