module Descriptor.LDT_Linkage (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.Link.ServiceInfo as ServiceInfo
import qualified Descriptor.LDT_Linkage.Item as Item

class (Base.Class a) => Class a where
  original_service_id :: a -> Word16
  transport_stream_id :: a -> Word16
  original_network_id :: a -> Word16
  items               :: a -> [Item.Data]

data Data = MkData {
  _header              :: Header.Data,
  _original_service_id :: Word16,
  _transport_stream_id :: Word16,
  _original_network_id :: Word16,
  _items               :: Vector Item.Data
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where

instance Class Data where
  original_service_id = _original_service_id
  transport_stream_id = _transport_stream_id
  original_network_id = _original_network_id
  items               = toList . _items 
