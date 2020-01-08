{-# LANGUAGE FlexibleInstances #-}

module SITables(
  CommonHeader,
  CommonHeader2
  ) where
  
import Data.Word(Word64, Word32, Word16, Word8)
import Descriptor(ContentBody(..))

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

data TransportStream = MkTransportStream {
  transport_stream_id :: Word16,
  original_network_id :: Word16,
  transport_descriptors_length :: Word16
  }

class (CommonHeader a ,CommonHeader2 a) => NIT a where
  network_id :: a -> Word16
  network_description_length :: a -> Word16
  transport_stream_loop_length :: a -> Word16

-- 14 bytes
-- transport_stream_id
-- original_network_id
-- service_id
class EIThead a where
    service_id                  :: a -> Word16 -- h->service_id = getBit(data, &boff, 16);
--    transport_stream_id         :: a -> Word16 -- h->transport_stream_id = getBit(data, &boff, 16);
--    original_network_id         :: a -> Word16 -- h->original_network_id = getBit(data, &boff, 16);
    segment_last_section_number :: a -> Word8 -- h->segment_last_section_number = getBit(data, &boff, 8);
    last_table_id               :: a -> Word8 -- h->last_table_id = getBit(data, &boff, 8);


-- 12 bytes 
class EITbody a where
  event_id                :: a -> Word16 -- 16
  start_time              :: a -> Word64 -- 40
  duration                :: a -> Word32 -- 24
  running_status          :: a -> Word8 -- 3
  free_CA_mode            :: a -> Bool -- 1
  descriotors_loop_length :: a -> Word16 -- 12

