module SITables.EIT(
  Data,
  Class(..),
  Item,
  pids,
  table_ids,
  parse,
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import Common(HasOriginalNetworkID(..),EmptyExist(..))
import Descriptor(HasServiceID(..))
import Parser(HasParser(startParse),ParseResult(..),ParseConditionSymbol(..),ValueCache,FromValueCache(..))
import qualified Descriptor
import Data.ByteString(ByteString)

pids :: [Word64]
pids = [0x0012,0x0026,0x0027]

table_ids :: [Word32]
table_ids = [0x4E,0x4F] ++ [0x50..0x5F] ++ [0x60..0x6F]

class (Header1.Class a, Header2.Class a, HasOriginalNetworkID a, HasServiceID a) => Class a where
  transport_stream_id         :: a -> Word16
  segment_last_section_number :: a -> Word8
  last_table_id               :: a -> Word8

data Data = MkData {
  -- CommonHeader 
  _table_id                    :: Word8, -- h->table_id = getBit(data, &boff, 8);
  _section_syntax_indicator    :: Bool, -- h->section_syntax_indicator = getBit(data, &boff, 1);
  _reserved_future_use         :: Bool, -- h->reserved_future_use = getBit(data, &boff, 1);
  _reserved1                   :: Word8, -- h->reserved1 = getBit(data, &boff, 2);
  _section_length              :: Word16, -- h->section_length =getBit(data, &boff,12);

  _service_id :: Word16,
  
  -- CommonHeader2  
  _reserved2                   :: Word8, -- h->reserved2 = getBit(data, &boff, 2);
  _version_number              :: Word8, -- h->version_number = getBit(data, &boff, 5);
  _current_next_indicator      :: Bool, -- h->current_next_indicator = getBit(data, &boff, 1);
  _section_number              :: Word8, -- h->section_number = getBit(data, &boff, 8);
  _last_section_number         :: Word8, -- h->last_section_number = getBit(data, &boff, 8);

  _transport_stream_id :: Word16,
  _original_network_id :: Word16,
  _segment_last_section_number :: Word8,
  _last_table_id               :: Word8
  }

instance EmptyExist Data where
  mkEmpty = MkData 0 False False 0 0 0 0 0 False 0 0 0 0 0 0

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

instance HasServiceID Data where
  service_id = _service_id

instance Class Data where
  transport_stream_id         = _transport_stream_id
  segment_last_section_number = _segment_last_section_number
  last_table_id               = _last_table_id

data Item = MkItem {
  event_id  :: Word16,
  start_time :: Word64,
  duration :: Word32,
  running_status :: Word8,
  free_CA_mode :: Bool,
  descriptors_loop_length :: Word16,
  _descriptors           :: [Descriptor.Data]
  }

instance HasDescriptors Item where
  descriptors = _descriptors

instance HasParser Data where

data Symbol = CommonHeader | ServiceID | CommonHeader2 | TransportStreamID | OriginalNetworkID | SegmentLastSectionNumber | LastTableID deriving (Eq,Enum,Bounded)

instance ParseConditionSymbol Symbol where
  getLen CommonHeader             = 24
  getLen ServiceID                = 16
  getLen CommonHeader2            = 24
  getLen TransportStreamID        = 16
  getLen OriginalNetworkID        = 16
  getLen SegmentLastSectionNumber = 8
  getLen LastTableID              = 8

update :: Symbol -> ValueCache -> Data -> Data
update CommonHeader  v old = old
update CommonHeader2 v old = old
update x v old =
  case x of
    ServiceID                -> old {_service_id                  = fromValueCache v}
    TransportStreamID        -> old {_transport_stream_id         = fromValueCache v}
    OriginalNetworkID        -> old {_original_network_id         = fromValueCache v}
    SegmentLastSectionNumber -> old {_segment_last_section_number = fromValueCache v}
    LastTableID              -> old {_last_table_id               = fromValueCache v}

result :: Data -> Maybe Data
result x = Just x
  
parse :: ByteString -> ParseResult Data
parse x = startParse x update result
