module Descriptor.EventGroup (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),HasMaybePrivateDataBytes(..),TOS(..),HasOriginalNetworkID(..),HasEventID(..),HasServiceID(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasMaybePrivateDataBytes a) => Class a where
  group_type       :: a -> Word8
  event_count      :: a -> Word8
  maybe_event      :: a -> Maybe [Info]
  maybe_group_data :: a -> Maybe [GroupInfo]
--  maybe_private_data_bytes :: a -> Maybe [Word8]  

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _group_type        :: Word8,
  _event_count       :: Word8,
  _maybe_event       :: Maybe [Info],
  _maybe_group_data  :: Maybe [GroupInfo],
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
  group_type       = _group_type
  event_count      = _event_count
  maybe_event      = _maybe_event
  maybe_group_data = _maybe_group_data

data Info = MkInfo {
  __service_id :: Word16,
  __event_id   :: Word16
  }
  
data GroupInfo = MkGroupInfo {
  _original_network_id :: Word16,
  _transport_stream_id :: Word16,
  _service_id          :: Word16,
  _event_id            :: Word16
  }

instance HasOriginalNetworkID GroupInfo where
  original_network_id = _original_network_id

instance TOS GroupInfo where
  transport_stream_id = _transport_stream_id
  
instance HasServiceID GroupInfo where
  service_id = _service_id

instance HasEventID GroupInfo where
  event_id = _event_id
