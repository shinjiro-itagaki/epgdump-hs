module Descriptor.HierarchicalTransmission where
-- import Descriptor(CountryCode)
import Descriptor.Common(Base)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a) => HierarchicalTransmission a where
  quality_level :: a -> Bool
  reference_pid :: a -> Word16
