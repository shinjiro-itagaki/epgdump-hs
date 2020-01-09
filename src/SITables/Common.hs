module SITables.Common(
  CommonHeader(..)
  ,CommonHeader2(..)
  ,HasDescriptors(..)
  -- TransportStream(
  --     transport_stream_id,
  --     original_network_id,
  --     transport_descriptors_length
  --     )
  ,Schedule(..)
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Descriptor

class CommonHeader a where
  table_id                    :: a -> Word8 -- h->table_id = getBit(data, &boff, 8);
  section_syntax_indicator    :: a -> Bool -- h->section_syntax_indicator = getBit(data, &boff, 1);
  reserved_future_use         :: a -> Bool -- h->reserved_future_use = getBit(data, &boff, 1);
  reserved1                   :: a -> Word8 -- h->reserved1 = getBit(data, &boff, 2);
  section_length              :: a -> Word16 -- h->section_length =getBit(data, &boff,12);

class CommonHeader2 a where
  reserved2                   :: a -> Word8 -- h->reserved2 = getBit(data, &boff, 2);
  version_number              :: a -> Word8 -- h->version_number = getBit(data, &boff, 5);
  current_next_indicator      :: a -> Bool -- h->current_next_indicator = getBit(data, &boff, 1);
  section_number              :: a -> Word8 -- h->section_number = getBit(data, &boff, 8);
  last_section_number         :: a -> Word8 -- h->last_section_number = getBit(data, &boff, 8);

class HasDescriptors a where
  descriptors :: a -> [Descriptor.Data]

--data TransportStream = MkTransportStream {
--  transport_stream_id :: Word16,
--  original_network_id :: Word16,
--  transport_descriptors_length :: Word16
--  }

class Schedule a where
  start_time :: a -> Word64
  duration   :: a -> Word32


