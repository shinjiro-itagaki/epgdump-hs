module SITables.PCAT (
  Data,
  Class(..),
  Item,
  pids,
  table_ids
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(CommonHeader(..) ,CommonHeader2(..),HasDescriptors(..),Schedule(..))
import Common(HasOriginalNetworkID(..))
import Descriptor(HasServiceID(..),HasEventID(..))
import qualified Descriptor

pids :: [Word64]
pids = [0x0022]

table_ids :: [Word32]
table_ids = [0xC2]

class (CommonHeader a ,CommonHeader2 a, HasServiceID a, HasOriginalNetworkID a) => Class a where
  transport_stream_id :: a -> Word16
  content_id          :: a -> Word32
  num_of_content_version :: a -> Word8

data Data = MkData {
  -- CommonHeader 
  _table_id                    :: Word8, -- h->table_id = getBit(data, &boff, 8);
  _section_syntax_indicator    :: Bool, -- h->section_syntax_indicator = getBit(data, &boff, 1);
  _reserved_future_use         :: Bool, -- h->reserved_future_use = getBit(data, &boff, 1);
  _reserved1                   :: Word8, -- h->reserved1 = getBit(data, &boff, 2);
  _section_length              :: Word16, -- h->section_length =getBit(data, &boff,12);

  _service_id                  :: Word16,
  
  -- CommonHeader2  
  _reserved2                   :: Word8, -- h->reserved2 = getBit(data, &boff, 2);
  _version_number              :: Word8, -- h->version_number = getBit(data, &boff, 5);
  _current_next_indicator      :: Bool, -- h->current_next_indicator = getBit(data, &boff, 1);
  _section_number              :: Word8, -- h->section_number = getBit(data, &boff, 8);
  _last_section_number         :: Word8, -- h->last_section_number = getBit(data, &boff, 8);

  _transport_stream_id    :: Word16,
  _original_network_id    :: Word16,
  _content_id             :: Word32,
  _num_of_content_version :: Word8
  }

instance CommonHeader Data where
  table_id                 = _table_id
  section_syntax_indicator = _section_syntax_indicator
  reserved_future_use      = _reserved_future_use
  reserved1                = _reserved1
  section_length           = _section_length
  
instance CommonHeader2 Data where
  reserved2                = _reserved2
  version_number           = _version_number
  current_next_indicator   = _current_next_indicator
  section_number           = _section_number
  last_section_number      = _last_section_number

instance HasOriginalNetworkID Data where
  original_network_id = _original_network_id

instance HasServiceID Data where
  service_id = _service_id

instance Class Data where
  transport_stream_id    = _transport_stream_id
  content_id             = _content_id
  num_of_content_version = _num_of_content_version
  
data ScheduleData = MkSchedule {
  _start_time :: Word64,
  _duration :: Word32
  }

instance Schedule ScheduleData where
  start_time = _start_time
  duration   = _duration

data Item = MkItem {
  content_version             :: Word16,
  content_minor_version       :: Word16,
  version_indicator           :: (Bool,Bool),
-- reserved_future_use        :: (Bool,Bool)
  content_descriptor_length   :: Word16,
-- reserved_future_use        :: Word8,
  schedule_description_length :: Word16,
  schedules                   :: [ScheduleData],
  _descriptors                :: [Descriptor.Data]
  }

instance HasDescriptors Item where
  descriptors = _descriptors
