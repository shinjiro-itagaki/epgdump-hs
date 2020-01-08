module Descriptor.ServiceGroup (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..),HasMaybePrivateDataBytes(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasMaybePrivateDataBytes a) => Class a where
  service_group_type :: a -> ServiceGroupType
--  reserved_future_use
  service_group_data :: a -> Maybe [ServicePair]
--  maybe_private_data_bytes :: a -> Maybe [Word8]
  
data ServiceGroupType = Simultaneous Word8 | Undefined Word8

data ServicePair = ServicePair {
  primary_service_id :: Word16,
  secondary_service_id :: Word16
  }

data Data = MkData {
  _descriptor_tag     :: Word8,
  _descriptor_length  :: Word8,
  _service_group_type :: ServiceGroupType,
  _service_group_data :: Maybe [ServicePair],
  _maybe_private_data_bytes :: Maybe [Word8]
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance HasMaybePrivateDataBytes Data where
  maybe_private_data_bytes = _maybe_private_data_bytes
  
instance Class Data where
  service_group_type = _service_group_type
  service_group_data = _service_group_data
