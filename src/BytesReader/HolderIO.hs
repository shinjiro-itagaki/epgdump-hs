module BytesReader.HolderIO where

import qualified BytesReader.Counter as Counter
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString,BytesLen)

class (Counter.Class a) => Class a where
  getBitsIO  :: (Integral i) => a -> i -> IO (Word64, a)
  getBytesIO :: (Integral i) => a -> i -> IO (ByteString, a)
  isEOF      :: a -> IO Bool

