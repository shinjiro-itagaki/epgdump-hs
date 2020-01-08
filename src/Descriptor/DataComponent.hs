module Descriptor.DataComponent (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),HasDataComponentID(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

-- import Descriptor(AreaCode)

class (Base a, HasDataComponentID a) => Class a where
--  data_component_id :: a -> Word16
  additional_data_component_info :: a -> [Word8]

data Data = MkData {
  _descriptor_tag        :: Word8,
  _descriptor_length     :: Word8,
  _data_component_id     :: Word16,
  _additional_data_component_info :: [Word8]
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance HasDataComponentID Data where
  data_component_id = _data_component_id

instance Class Data where
  additional_data_component_info = _additional_data_component_info 
