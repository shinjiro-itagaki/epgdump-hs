module Descriptor.PartialReception where
import Descriptor.Common(Base,AreaCode)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)


class Base a => PartialReception a where
  service_ids :: a -> [Word16]

