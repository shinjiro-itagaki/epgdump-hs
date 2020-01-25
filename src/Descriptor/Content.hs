module Descriptor.Content (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Data.ByteString.Lazy as BS

import qualified Descriptor.Content.Item as Item

class Base.Class a => Class a where
  items :: a -> [Item.Data]

data Data = MkData {
  _header :: Header.Data,
  _items :: Vector Item.Data
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
  fromByteStringAfterHeader h bs =
    let (items,rest) = Base.gather (Header.descriptor_length h) impl snoc bs empty
    in (Just $ MkData h items, rest)
    where
      impl :: ByteString -> (Maybe Item.Data, ByteString)
      impl xs = let (bs,rest) = BS.splitAt 2 xs
                in (Just $ Item.mk $ BS.unpack bs, rest)

instance Class Data where
  items = toList . _items
