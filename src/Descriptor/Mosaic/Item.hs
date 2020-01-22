module Descriptor.Mosaic.Item where
import qualified Descriptor.Mosaic.ElementaryCellField as ElementaryCellField
import qualified Descriptor.Mosaic.CellLinkageInfo as CellLinkageInfo
import Data.Vector(Vector,toList,empty,snoc)
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString)

class Class a where
  logical_cell_id                :: a -> Word8 -- 6
  reserved_future_use            :: a -> Word8 -- 7
  logical_cell_presentation_info :: a -> Word8 -- 3
  elementary_cell_field_length   :: a -> Word8 -- 8
  elementary_cell_fields         :: a -> Vector ElementaryCellField.Data
  cell_linkage_info              :: a -> Word8
  info                           :: a -> CellLinkageInfo.Data  

data Data = MkData {
  _logical_cell_id                :: Word8, -- 6
  _reserved_future_use            :: Word8, -- 7
  _logical_cell_presentation_info :: Word8, -- 3
  _elementary_cell_field_length   :: Word8, -- 8
  _elementary_cell_fields         :: Vector ElementaryCellField.Data,
  _cell_linkage_info              :: Word8, -- 8
  _info                           :: CellLinkageInfo.Data
  } deriving (Show)

instance Class Data where
  logical_cell_id                = _logical_cell_id
  reserved_future_use            = _reserved_future_use
  logical_cell_presentation_info = _logical_cell_presentation_info
  elementary_cell_field_length   = _elementary_cell_field_length
  elementary_cell_fields         = _elementary_cell_fields
  cell_linkage_info              = _cell_linkage_info
  info                           = _info
