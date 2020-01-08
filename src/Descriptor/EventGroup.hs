module Descriptor.EventGroup where
import Descriptor.Common(Base,HasMaybePrivateDataBytes,TOS(..),HasOriginalNetworkID(..),HasEventID(..),HasServiceID(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasMaybePrivateDataBytes a) => EventGroup a where
  group_type :: a -> Word8
  event_count :: a -> Word8
  maybe_event :: a -> Maybe [Event]
  maybe_group_data :: a -> Maybe [EventGroupData]
--  maybe_private_data_bytes :: a -> Maybe [Word8]  


data Event = MkEvent {
  _event_service_id :: Word16,
  _event_event_id :: Word16
  }
  
data EventGroupData = MkEventGroupData {
  _event_group_original_network_id :: Word16,
  _event_group_transport_stream_id :: Word16,
  _event_group_service_id :: Word16,
  _event_group_event_id :: Word16
  }

instance HasOriginalNetworkID EventGroupData where
  original_network_id = _event_group_original_network_id

instance TOS EventGroupData where
  transport_stream_id = _event_group_transport_stream_id
  
instance HasServiceID EventGroupData where
  service_id = _event_group_service_id

instance HasEventID EventGroupData where
  event_id = _event_group_event_id
