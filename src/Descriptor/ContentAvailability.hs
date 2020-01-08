module Descriptor.ContentAvailability where
import Descriptor.Common(Base)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => ContentAvailability a where
  copy_restriction_mode :: a -> Bool
  image_constraint_token :: a -> Bool
  retention_mode :: a -> Bool
  retention_state :: a -> Word8
  encryption_mode :: a -> Bool
-- reserved_future_use N  
 

