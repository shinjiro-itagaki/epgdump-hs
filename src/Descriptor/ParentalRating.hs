module Descriptor.ParentalRating (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.ParentalRating.Item as Item
import Data.Vector(Vector,empty,toList,snoc)

class Base.Class a => Class a where
  ratings :: a -> [Item.Data]

data Data = MkData {
  _ratings :: Vector Item.Data
  }


instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  ratings = toList . _ratings
