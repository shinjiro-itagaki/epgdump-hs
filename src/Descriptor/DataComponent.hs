module Descriptor.DataComponent where
import Descriptor.Common(Base,HasDataComponentID)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

-- import Descriptor(AreaCode)

class (Base a, HasDataComponentID a) => DataComponent a where
--  data_component_id :: a -> Word16
  additional_data_component_info :: a -> [Word8]

