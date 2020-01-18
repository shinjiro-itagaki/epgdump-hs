module Descriptor.LocalTimeOffset (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.LocalTimeOffset.Item as Item

class (Base.Class a) => Class a where
  items :: a -> [Item.Data]

data Data = MkData {
  _items :: Vector Item.Data
  }

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  items = toList . _items
