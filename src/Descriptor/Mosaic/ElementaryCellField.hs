module Descriptor.Mosaic.ElementaryCellField where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Utils.FromByteString as FromByteString
import Data.Bits((.&.))

class (FromByteString.Class a) => Class a where
  reserved_future_use :: a -> (Bool,Bool) -- 2
  elementary_cell_id  :: a -> Word8 --6
  
data Data = MkData {
  _reserved_future_use :: (Bool,Bool), -- 2
  _elementary_cell_id  :: Word8 --6
  } deriving (Show,Eq)

mk :: Word8 -> Data
mk x = MkData {
  _reserved_future_use = ((x .&. 0x02) /= 0,(x .&. 0x01) /= 0), -- 2
  _elementary_cell_id  = x .&. 0x3F
  }

instance FromByteString.Class Data where
  fromByteStringWithRest = FromByteString.map mk . FromByteString.fromByteStringWithRest

instance Class Data where
  reserved_future_use = _reserved_future_use 
  elementary_cell_id  = _elementary_cell_id 
