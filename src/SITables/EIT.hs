module SITables.EIT where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(CommonHeader(..) ,CommonHeader2(..))

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


