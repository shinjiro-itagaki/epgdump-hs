module Descriptor.TerrestrialDeliverySystem (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..),AreaCode)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  area_code         :: a -> AreaCode
  guard_interval    :: a -> Word8
  transmission_mode :: a -> Word8
  frequencies       :: a -> [Word16]

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _area_code         :: AreaCode,
  _guard_interval    :: Word8,
  _transmission_mode :: Word8,
  _frequencies       :: [Word16]
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  area_code         = _area_code
  guard_interval    = _guard_interval
  transmission_mode = _transmission_mode
  frequencies       = _frequencies
