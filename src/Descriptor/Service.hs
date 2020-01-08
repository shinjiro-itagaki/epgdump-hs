module Descriptor.Service where
import Descriptor.Common(Base,HasServiceID(..),HasServiceType(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasServiceType a) => Service a where
--  service_type :: a -> Word8
  service_provider_name_length :: a -> Word8
  service_provider_name :: a -> String
  service_name_length :: a -> Word8
  service_name :: a -> String


data ServiceData = MkServiceData {
  _service_id :: Word16,
  _service_type :: Word8
  }

instance HasServiceID ServiceData where
  service_id = _service_id

instance HasServiceType ServiceData where
  service_type = _service_type
