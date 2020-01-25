module Descriptor.DataComponent (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)

-- import Descriptor(AreaCode)

class (Base.Class a) => Class a where
  data_component_id              :: a -> Word16
  additional_data_component_info :: a -> [Word8]

data Data = MkData {
  _header :: Header.Data,
  _data_component_id     :: Word16,
  _additional_data_component_info :: [Word8]
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  data_component_id              = _data_component_id
  additional_data_component_info = _additional_data_component_info 
