module Descriptor.SystemManagement (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  broadcasting_flag                  :: a -> Word8
  broadcasting_identifier            :: a -> Word8
  additional_broadcasting_identifier :: a -> Word8
  additional_identification_info     :: a -> [Word8]

data Data = MkData {
  _descriptor_tag                     :: Word8,
  _descriptor_length                  :: Word8,
  _broadcasting_flag                  :: Word8,
  _broadcasting_identifier            :: Word8,
  _additional_broadcasting_identifier :: Word8,
  _additional_identification_info     :: [Word8]
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  broadcasting_flag                  = _broadcasting_flag
  broadcasting_identifier            = _broadcasting_identifier
  additional_broadcasting_identifier = _additional_broadcasting_identifier
  additional_identification_info     = _additional_identification_info
