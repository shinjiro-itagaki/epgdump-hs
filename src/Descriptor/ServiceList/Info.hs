module Descriptor.ServiceList.Info where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)

class Class a where
  service_id   :: a -> Word16
  service_type :: a -> Word8

data Data = MkData {
  _service_id   :: Word16,
  _service_type :: Word8
  } deriving (Show)

instance Class Data where
  service_id   = _service_id
  service_type = _service_type
