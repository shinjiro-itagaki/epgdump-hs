module Descriptor.CAIdentifier (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  ca_system_id :: a -> Word16
  
data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _ca_system_id      :: Word16
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where  
  ca_system_id = _ca_system_id

