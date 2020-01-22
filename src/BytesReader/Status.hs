{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module BytesReader.Status(
  Data
  ,Class(..)
  ,new
  ) where
  
import Common(ByteString,BytesLen)
import Data.Word(Word64, Word32, Word16, Word8)

class Class a where
  pos :: a -> BytesLen
  size :: a -> BytesLen
  progress :: a -> Rational
  progress x = (toRational $ pos x) / (toRational $ size x)

  progress_percent :: (Fractional b) => a -> b
  progress_percent = fromRational . (* 100) . progress
  
  getStatus :: a -> Data
  getStatus x = new (pos x) (size x)
  
data Data = MkData {
  _pos :: Word64,
  _size :: BytesLen
  } deriving (Show)

new :: BytesLen -> BytesLen -> Data
new p s =  MkData {
  _pos  = p,
  _size = s
  }

instance Class Data where
  pos = _pos
  size = _size
