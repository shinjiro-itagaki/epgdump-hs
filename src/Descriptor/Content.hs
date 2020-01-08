module Descriptor.Content where
import Descriptor.Common(Base)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Content a where
  body :: a -> [ContentBody]

data ContentBody = MkContentBody {
  content_nibble_level_1 :: Word8,
  content_nibble_level_2 :: Word8,
  user_nibble_1 :: Word8,
  user_nibble_2 :: Word8
  }
