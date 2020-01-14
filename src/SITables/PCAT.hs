module SITables.PCAT (
  Data,
  Class(..),
  Item,
  pids,
  table_ids
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..),Schedule(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import Common(HasOriginalNetworkID(..))
import Descriptor(HasServiceID(..),HasEventID(..))
import qualified Descriptor

pids :: [Word64]
pids = [0x0022]

table_ids :: [Word32]
table_ids = [0xC2]

class (Header1.Class a, Header2.Class a, HasServiceID a, HasOriginalNetworkID a) => Class a where
  transport_stream_id :: a -> Word16
  content_id          :: a -> Word32
  num_of_content_version :: a -> Word8

data Data = MkData {
  _header1    :: Header1.Data,
  _service_id :: Word16,
  _header2    :: Header2.Data,

  _transport_stream_id    :: Word16,
  _original_network_id    :: Word16,
  _content_id             :: Word32,
  _num_of_content_version :: Word8
  }

instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2

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
