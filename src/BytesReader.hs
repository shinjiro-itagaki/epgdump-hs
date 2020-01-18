{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module BytesReader where
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Lazy(pack,unpack,null,uncons,empty)
import Data.Bits((.&.),shiftL,shiftR)
import Data.Char(chr)
import Common(ByteString,BytesLen)

class Counter a where
  getBytesCounter   :: a -> BytesLen
  resetBytesCounter :: a -> a

class (Counter a) => Holder a where
  getBits  :: (Integral i) => a -> i -> (Word64, a)
  getBytes :: (Integral i) => a -> i -> (ByteString, a)
  
class (Counter a) => HolderIO a where
  getBitsIO  :: (Integral i) => a -> i -> IO (Word64, a)
  getBytesIO :: (Integral i) => a -> i -> IO (ByteString, a)
  isEOF      :: a -> IO Bool

