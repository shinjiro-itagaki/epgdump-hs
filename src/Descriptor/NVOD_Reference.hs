module Descriptor.NVOD_Reference where
import Descriptor.Common(Base,TOSData)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => NVOD_Reference a where
  references :: a -> [TOSData]
  
