module Descriptor.Mosaic where
import Descriptor.Common(Base,HasComponentTag,TOSData,HasOriginalNetworkID(..),TOS(..),HasServiceID(..),HasEventID(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a) => Mosaic a where
  mosaic_entry_point :: a -> Bool
  number_of_horizontal_elementary_cells :: a -> Word8
-- reserved_future_use :: a -> Word8
  number_of_vertical_elementary_cells :: a -> Word8
  mosaic_items :: a -> [MosaicItem]


data ElementaryCellField = MkElementaryCellField {
--  reserved_future_use
  elementary_cell_id :: Word8
    }

-- class TOS a => MosaicItem a where
data MosaicItem = MkMosaicItem {
  logical_cell_id :: Word8,
--  reserved_future_use,
  logical_cell_presentation_info :: Word8,
  elementary_cell_field_length :: Word8,
  elementary_cell_fields :: [ElementaryCellField],
  cell_linkage_info :: Word8,
  bouquet_id :: Maybe Word16,
--  original_network_id :: Word16,
--  transport_stream_id :: Word16,
--  service_id :: Word16,
  _mosaic_item_event_id :: Word16,
  _tosdata :: TOSData -- 非公開
  }
  
instance HasOriginalNetworkID MosaicItem where
  original_network_id = original_network_id . _tosdata

instance TOS MosaicItem where
  transport_stream_id = transport_stream_id . _tosdata
  
instance HasServiceID MosaicItem where  
  service_id = service_id . _tosdata

instance HasEventID MosaicItem where
  event_id = _mosaic_item_event_id
