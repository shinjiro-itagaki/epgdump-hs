module SITables.BIT (
  Data,
  Class(..),
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
pids = [0x0024]

table_ids :: [Word32]
table_ids = [0xC4]

class (Header1.Class a ,Header2.Class a, HasOriginalNetworkID a) => Class a where
  broadcast_view_propriety :: a -> Bool
  first_descriptors_length :: a -> Word16
  first_descriptors        :: a -> [Descriptor.Data]
  broadcaster_id           :: a -> Word8
  broadcaster_descriptor_length :: a -> Word16
  broadcaster_descriptors  :: a -> [Descriptor.Data]

data Data = MkData {
  -- CommonHeader 
  _table_id                    :: Word8, -- h->table_id = getBit(data, &boff, 8);
  _section_syntax_indicator    :: Bool, -- h->section_syntax_indicator = getBit(data, &boff, 1);
  _reserved_future_use         :: Bool, -- h->reserved_future_use = getBit(data, &boff, 1);
  _reserved1                   :: Word8, -- h->reserved1 = getBit(data, &boff, 2);
  _section_length              :: Word16, -- h->section_length =getBit(data, &boff,12);

  _original_network_id    :: Word16,  
  
  -- CommonHeader2  
  _reserved2                   :: Word8, -- h->reserved2 = getBit(data, &boff, 2);
  _version_number              :: Word8, -- h->version_number = getBit(data, &boff, 5);
  _current_next_indicator      :: Bool, -- h->current_next_indicator = getBit(data, &boff, 1);
  _section_number              :: Word8, -- h->section_number = getBit(data, &boff, 8);
  _last_section_number         :: Word8, -- h->last_section_number = getBit(data, &boff, 8);

  _broadcast_view_propriety      :: Bool,
  _first_descriptors_length      :: Word16,
  _first_descriptors             :: [Descriptor.Data],
  _broadcaster_id                :: Word8,
  _broadcaster_descriptor_length :: Word16,
  _broadcaster_descriptors       :: [Descriptor.Data]
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

instance HasOriginalNetworkID Data where
  original_network_id = _original_network_id

instance Class Data where
  broadcast_view_propriety = _broadcast_view_propriety 
  first_descriptors_length = _first_descriptors_length 
  first_descriptors        = _first_descriptors        
  broadcaster_id           = _broadcaster_id           
  broadcaster_descriptor_length = _broadcaster_descriptor_length
  broadcaster_descriptors  = _broadcaster_descriptors  
