module Descriptor.TerrestrialDeliverySystem where
import Descriptor.Common(Base,AreaCode)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => TerrestrialDeliverySystem a where
  area_code :: a -> AreaCode
  guard_interval :: a -> Word8
  transmission_mode :: a -> Word8
  frequencies :: a -> [Word16]


