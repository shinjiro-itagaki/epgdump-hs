{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module BytesReader.Handler where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString,BytesLen)

class Class a where
  hGet  :: (Integral i) => a -> i -> IO ByteString
  isEOF :: a -> IO Bool
  size  :: a -> IO BytesLen
