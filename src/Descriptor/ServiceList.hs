module Descriptor.ServiceList where
import Descriptor.Common(Base)
import Descriptor.Service(ServiceData)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => ServiceList a where
  list :: a -> [ServiceData]

