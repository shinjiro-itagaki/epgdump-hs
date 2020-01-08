module Descriptor.ServiceGroup where
import Descriptor.Common(Base,HasMaybePrivateDataBytes)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasMaybePrivateDataBytes a) => ServiceGroup a where
  service_group_type :: a -> ServiceGroupType
--  reserved_future_use
  service_group_data :: a -> Maybe [ServicePair]
--  maybe_private_data_bytes :: a -> Maybe [Word8]
  
data ServiceGroupType = Simultaneous Word8 | Undefined Word8

data ServicePair = ServicePair {
  primary_service_id :: Word16,
  secondary_service_id :: Word16
  }

