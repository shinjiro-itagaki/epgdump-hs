module SITables.NIT(
  Data,
  Class(..),
  Item  
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import Common(HasOriginalNetworkID(..){-,HasParser(..)-})
import qualified Descriptor

pids :: [Word64]
pids = [0x0010]

table_ids :: [Word32]
table_ids = [0x40,0x41]

class (Header1.Class a, Header2.Class a, HasDescriptors a) => Class a where
  network_id                   :: a -> Word16
  network_descriptors_length   :: a -> Word16
  transport_stream_loop_length :: a -> Word16
  transport_streams            :: a -> [Item]

data Data = MkData {
  -- CommonHeader 
  _table_id                    :: Word8, -- h->table_id = getBit(data, &boff, 8);
  _section_syntax_indicator    :: Bool, -- h->section_syntax_indicator = getBit(data, &boff, 1);
  _reserved_future_use         :: Bool, -- h->reserved_future_use = getBit(data, &boff, 1);
  _reserved1                   :: Word8, -- h->reserved1 = getBit(data, &boff, 2);
  _section_length              :: Word16, -- h->section_length =getBit(data, &boff,12);

  _network_id                   :: Word16,
  _network_descriptors_length   :: Word16,
  _descriptors                  :: [Descriptor.Data],
  _transport_stream_loop_length :: Word16,
  _transport_streams            :: [Item],
  
  -- CommonHeader2  
  _reserved2                   :: Word8, -- h->reserved2 = getBit(data, &boff, 2);
  _version_number              :: Word8, -- h->version_number = getBit(data, &boff, 5);
  _current_next_indicator      :: Bool, -- h->current_next_indicator = getBit(data, &boff, 1);
  _section_number              :: Word8, -- h->section_number = getBit(data, &boff, 8);
  _last_section_number         :: Word8 -- h->last_section_number = getBit(data, &boff, 8);    
  }

instance Header1.Class Data where
  table_id                 = _table_id
  section_syntax_indicator = _section_syntax_indicator
  reserved_future_use      = _reserved_future_use
  reserved1                = _reserved1
  section_length           = _section_length
  
instance Header2.Class Data where
  reserved2                = _reserved2
  version_number           = _version_number
  current_next_indicator   = _current_next_indicator
  section_number           = _section_number
  last_section_number      = _last_section_number

instance HasDescriptors Data where
  descriptors = _descriptors

instance Class Data where
  network_id                   = _network_id
  network_descriptors_length   = _network_descriptors_length
  transport_stream_loop_length = _transport_stream_loop_length
  transport_streams            = _transport_streams
  

data Item = MkItem {
  transport_stream_id          :: Word16,
  _original_network_id         :: Word16,
  transport_descriptors_length :: Word16,
  __descriptors                :: [Descriptor.Data]
  }

instance HasOriginalNetworkID Item where
  original_network_id = _original_network_id

instance HasDescriptors Item where
  descriptors = __descriptors

--instance HasParser Data where
--  parse :: ByteString -> ParseResult a