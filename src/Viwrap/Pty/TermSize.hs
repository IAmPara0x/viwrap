{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
module Viwrap.Pty.TermSize
  ( TermSize (..)
  , getTermSize
  , setTermSize
  ) where

import Control.Monad         (void)

import Foreign.C.Types       (CInt (..), CUShort)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Ptr           (Ptr)
import Foreign.Storable      (Storable (..))

import System.Posix          (Fd (Fd))

import Text.Printf           (printf)

pattern TIOCGWINSZ :: CInt
pattern TIOCGWINSZ = (21523 :: CInt)

pattern TIOCSCTTY :: CInt
pattern TIOCSCTTY = (21524 :: CInt)

data CWinsize
  = CWinsize
      { c_wsRow    :: CUShort
      , c_wsCol    :: CUShort
      , c_wsxpixel :: CUShort
      , c_wsypixel :: CUShort
      }

instance Storable CWinsize where
  sizeOf _ = sizeOf (undefined :: CUShort) * 4
  alignment = sizeOf
  peek ptr =
    CWinsize <$> peekByteOff ptr 0 <*> peekByteOff ptr 2 <*> peekByteOff ptr 4 <*> peekByteOff ptr 6
  poke ptr (CWinsize {..}) = do
    pokeByteOff ptr 0 c_wsRow
    pokeByteOff ptr 2 c_wsCol
    pokeByteOff ptr 4 c_wsxpixel
    pokeByteOff ptr 6 c_wsypixel

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr a -> IO CInt

data TermSize
  = TermSize
      { termWidth  :: Int
      , termHeight :: Int
      }
  deriving stock (Show)

getTermSize :: Fd -> IO TermSize
getTermSize (Fd fd) = do
  ptr_ws <- malloc
  code   <- c_ioctl fd TIOCGWINSZ ptr_ws

  if code == 0
    then do
      CWinsize { c_wsRow, c_wsCol } <- peek ptr_ws

      let termSize :: TermSize
          termSize = TermSize { termWidth  = fromInteger $ toInteger c_wsCol
                              , termHeight = fromInteger $ toInteger c_wsRow
                              }
      free ptr_ws
      return termSize
    else do
      free ptr_ws
      error (printf "ERROR: while calling ioctl to get the terminal size for %s" (show fd))


setTermSize :: Fd -> Fd -> IO ()
setTermSize (Fd fromFd) (Fd toFd) = do
  ptr_ws <- (malloc @CWinsize)

  void $ c_ioctl fromFd TIOCGWINSZ ptr_ws

  void $ c_ioctl toFd TIOCSCTTY ptr_ws

  free ptr_ws

