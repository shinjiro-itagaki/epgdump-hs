module Descriptor.HierarchicalTransmission (
  Class(..)
  ,Data
  ) where
-- import Descriptor(CountryCode)
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a) => Class a where
  quality_level :: a -> Bool
  reference_pid :: a -> Word16

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _quality_level     :: Bool,
  _reference_pid     :: Word16
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where  
  quality_level = _quality_level
  reference_pid = _reference_pid
