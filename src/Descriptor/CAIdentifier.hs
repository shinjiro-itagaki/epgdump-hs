module Descriptor.CAIdentifier where
import Descriptor.Common(Base)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => CAIdentifier a where
  ca_system_id :: a -> Word16


