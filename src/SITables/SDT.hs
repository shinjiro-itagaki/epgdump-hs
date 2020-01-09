module SITables.SDT where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(CommonHeader(..) ,CommonHeader2(..),HasDescriptors(..))
import Common(HasOriginalNetworkID(..))
import qualified Descriptor

class (CommonHeader a ,CommonHeader2 a, HasOriginalNetworkID a) => Class a where
  transport_stream_id :: a -> Word16

data Data = MkData {
  -- CommonHeader 
  _table_id                    :: Word8, -- h->table_id = getBit(data, &boff, 8);
  _section_syntax_indicator    :: Bool, -- h->section_syntax_indicator = getBit(data, &boff, 1);
  _reserved_future_use         :: Bool, -- h->reserved_future_use = getBit(data, &boff, 1);
  _reserved1                   :: Word8, -- h->reserved1 = getBit(data, &boff, 2);
  _section_length              :: Word16, -- h->section_length =getBit(data, &boff,12);

  _transport_stream_id :: Word16,
  
  -- CommonHeader2  
  _reserved2                   :: Word8, -- h->reserved2 = getBit(data, &boff, 2);
  _version_number              :: Word8, -- h->version_number = getBit(data, &boff, 5);
  _current_next_indicator      :: Bool, -- h->current_next_indicator = getBit(data, &boff, 1);
  _section_number              :: Word8, -- h->section_number = getBit(data, &boff, 8);
  _last_section_number         :: Word8, -- h->last_section_number = getBit(data, &boff, 8);
  
  _original_network_id :: Word16  
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

instance Class Data where
  transport_stream_id = _transport_stream_id

data Item = MkItem {
  _service_id  :: Word16,
  eit_user_defined_flags :: Word8,
  eit_schedule_flag :: Bool,
  eit_present_following_flag :: Bool,
  running_status :: Word8,
  free_CA_mode :: Bool,
  descriptors_loop_length :: Word16,
  __descriptors                :: [Descriptor.Data]
  }

instance HasDescriptors Item where
  descriptors = __descriptors

instance Descriptor.HasServiceID Item where
  service_id = _service_id
