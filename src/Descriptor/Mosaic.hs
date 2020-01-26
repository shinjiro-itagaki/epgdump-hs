-- 6.2.9
module Descriptor.Mosaic (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Data.ByteString.Lazy as BS
import qualified Descriptor.Mosaic.Item as Item
import Data.Bits(shiftR,(.&.))
import Utils.FromByteString(fromByteStringWithRest,fromByteString, gatherFromByteString)
import qualified Parser.Result as Result

class (Base.Class a) => Class a where
  mosaic_entry_point                    :: a -> Bool
  number_of_horizontal_elementary_cells :: a -> Word8
  reserved_future_use                   :: a -> Bool
  number_of_vertical_elementary_cells   :: a -> Word8
  items                                 :: a -> [Item.Data]

data Data = MkData {
  _header                                :: Header.Data, 
  _mosaic_entry_point                    :: Bool,
  _number_of_horizontal_elementary_cells :: Word8,
  _reserved_future_use                   :: Bool, 
  _number_of_vertical_elementary_cells   :: Word8,
  _items                                 :: Vector Item.Data
  } deriving (Show)

instance Header.Class Data where
  header = _header
  
instance Base.Class Data where
  fromByteStringAfterHeader h bs =
    let (bs0,rest) = BS.splitAt (Header.descriptor_length h) bs
        (w8,bs1)   = fromByteStringWithRest bs0
        
        mosaic_entry_point                    = (w8 .&. 0x80) /= 0
        number_of_horizontal_elementary_cells =  w8 .&. 0x70
        reserved_future_use                   = (w8 .&. 0x08) /= 0
        number_of_vertical_elementary_cells   =  w8 .&. 0x07
        items                                 = gatherFromByteString bs1 snoc
        d = MkData {
          _header                                = h,
          _mosaic_entry_point                    = mosaic_entry_point,
          _number_of_horizontal_elementary_cells = number_of_horizontal_elementary_cells,
          _reserved_future_use                   = reserved_future_use,
          _number_of_vertical_elementary_cells   = number_of_vertical_elementary_cells,
          _items                                 = items
          }
    in Result.Parsed d
        
instance Class Data where
  mosaic_entry_point                    = _mosaic_entry_point
  number_of_horizontal_elementary_cells = _number_of_horizontal_elementary_cells
  number_of_vertical_elementary_cells   = _number_of_vertical_elementary_cells
  items                                 = toList . _items
