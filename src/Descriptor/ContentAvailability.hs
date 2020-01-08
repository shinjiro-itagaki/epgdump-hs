module Descriptor.ContentAvailability (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  copy_restriction_mode  :: a -> Bool
  image_constraint_token :: a -> Bool
  retention_mode         :: a -> Bool
  retention_state        :: a -> Word8
  encryption_mode        :: a -> Bool
-- reserved_future_use N  

data Data = MkData {
  _descriptor_tag         :: Word8,
  _descriptor_length      :: Word8,
  _copy_restriction_mode  :: Bool,
  _image_constraint_token :: Bool,
  _retention_mode         :: Bool,
  _retention_state        :: Word8,
  _encryption_mode        :: Bool
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  copy_restriction_mode  = _copy_restriction_mode
  image_constraint_token = _image_constraint_token
  retention_mode         = _retention_mode
  retention_state        = _retention_state
  encryption_mode        = _encryption_mode
