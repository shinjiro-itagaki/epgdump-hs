module Descriptor.Content (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  body :: a -> [Item]

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _body              :: [Item]
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  body = _body

data Item = MkItem {
  content_nibble_level_1 :: Word8,
  content_nibble_level_2 :: Word8,
  user_nibble_1 :: Word8,
  user_nibble_2 :: Word8
  }
