module SITables.RST(
  Data(sections),
  Class(..),
  Item,
  pids, table_ids
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(CommonHeader(..))
import Common(HasOriginalNetworkID(..))
import Descriptor(HasServiceID(..),HasEventID(..))
import qualified Descriptor

-- pids, table_ids
pids :: [Word64]
pids = [0x0013]

table_ids :: [Word32]
table_ids = [0x71]

class (CommonHeader a) => Class a where

data Data = MkData {
  -- CommonHeader 
  _table_id                    :: Word8, -- h->table_id = getBit(data, &boff, 8);
  _section_syntax_indicator    :: Bool, -- h->section_syntax_indicator = getBit(data, &boff, 1);
  _reserved_future_use         :: Bool, -- h->reserved_future_use = getBit(data, &boff, 1);
  _reserved1                   :: Word8, -- h->reserved1 = getBit(data, &boff, 2);
  _section_length              :: Word16, -- h->section_length =getBit(data, &boff,12);
  sections                     :: [Item]
  }

instance CommonHeader Data where
  table_id                 = _table_id
  section_syntax_indicator = _section_syntax_indicator
  reserved_future_use      = _reserved_future_use
  reserved1                = _reserved1
  section_length           = _section_length

data Item = MkItem {
  transport_stream_id  :: Word16,
  _original_network_id :: Word16,
  _service_id          :: Word16,
  _event_id            :: Word16,
  running_status       :: Word16
  }

instance HasOriginalNetworkID Item where
  original_network_id = _original_network_id

instance HasServiceID Item where
  service_id = service_id

instance HasEventID Item where
  event_id = event_id  
