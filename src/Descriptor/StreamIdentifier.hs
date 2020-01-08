module Descriptor.StreamIdentifier where
import Descriptor.Common(Base,HasComponentTag)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasComponentTag a) => StreamIdentifier a where
--  component_tag :: a -> Word8
  

