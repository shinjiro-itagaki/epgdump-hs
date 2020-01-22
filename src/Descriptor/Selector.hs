module Descriptor.Selector where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
class Class a where
  selector_length :: a -> Word8
  selector_bytes  :: a -> ByteString

data Data = MkData {
  _selector_length :: Word8,
  _selector_bytes  :: ByteString
  } deriving (Show)

instance Class Data where
  selector_length = _selector_length 
  selector_bytes  = _selector_bytes
