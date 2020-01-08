module Descriptor.TimeShiftedService (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..),HasReferenceServiceID(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasReferenceServiceID a) => Class a where
--  reference_service_id :: a -> Word16

data Data = MkData {
  _descriptor_tag         :: Word8,
  _descriptor_length      :: Word8,
  _reference_service_id   :: Word16
  }
  
instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance HasReferenceServiceID Data where
  reference_service_id = _reference_service_id
