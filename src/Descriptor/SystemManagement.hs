module Descriptor.SystemManagement where
import Descriptor.Common(Base)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => SystemManagement a where
  broadcasting_flag :: a -> Word8
  broadcasting_identifier :: a -> Word8
  additional_broadcasting_identifier :: a -> Word8
  additional_identification_info :: a -> [Word8]

