module Descriptor.Mosaic (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.Mosaic.Item as Item

class (Base.Class a) => Class a where
  mosaic_entry_point                    :: a -> Bool
  number_of_horizontal_elementary_cells :: a -> Word8
  reserved_future_use                   :: a -> Word8
  number_of_vertical_elementary_cells   :: a -> Word8
  items                                 :: a -> [Item.Data]

data Data = MkData {
  _header                                :: Header.Data, 
  _mosaic_entry_point                    :: Bool,
  _number_of_horizontal_elementary_cells :: Word8,
  _number_of_vertical_elementary_cells   :: Word8,
  _items                                 :: Vector Item.Data
  }

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  mosaic_entry_point                    = _mosaic_entry_point
  number_of_horizontal_elementary_cells = _number_of_horizontal_elementary_cells
  number_of_vertical_elementary_cells   = _number_of_vertical_elementary_cells
  items                                 = toList . _items
