module Descriptor.Mosaic.ElementaryCellField where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString)


class Class a where
  reserved_future_use :: a -> (Bool,Bool) -- 2
  elementary_cell_id  :: a -> Word8 --6
  
data Data = MkData {
  _reserved_future_use :: (Bool,Bool), -- 2
  _elementary_cell_id  :: Word8 --6
  } deriving (Show)

instance Class Data where
  reserved_future_use = _reserved_future_use 
  elementary_cell_id  = _elementary_cell_id 
