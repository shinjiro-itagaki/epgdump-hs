module Descriptor.Service (
  Class(..)
  ,Data
  ,Info
  ) where
import Descriptor.Common(Base(..),HasServiceID(..),HasServiceType(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasServiceType a) => Class a where
--  service_type :: a -> Word8
  service_provider_name_length :: a -> Word8
  service_provider_name        :: a -> String
  service_name_length          :: a -> Word8
  service_name                 :: a -> String

data Data = MkData {
  _descriptor_tag               :: Word8,
  _descriptor_length            :: Word8,
  _service_type                 :: Word8,
  _service_provider_name_length :: Word8,
  _service_provider_name        :: String,
  _service_name_length          :: Word8,
  _service_name                 :: String
  }

data Info = MkInfo {
  __service_id   :: Word16,
  __service_type :: Word8
  }

instance HasServiceID Info where
  service_id = __service_id

instance HasServiceType Info where
  service_type = __service_type

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance HasServiceType Data where
  service_type = _service_type
  
instance Class Data where
  service_provider_name_length  = _service_provider_name_length
  service_provider_name         = _service_provider_name
  service_name_length           = _service_name_length
  service_name                  = _service_name
