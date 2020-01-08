module Descriptor.LogoTransmission (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  logo_transmission_type :: a -> Word8
  logo_id                :: a -> Maybe Word16
  logo_version           :: a -> Maybe Word16
  download_data_id       :: a -> Maybe Word16
  logo_char              :: a -> Maybe String

data Data = MkData {
  _descriptor_tag         :: Word8,
  _descriptor_length      :: Word8,
  _logo_transmission_type :: Word8,
  _logo_id                :: Maybe Word16,
  _logo_version           :: Maybe Word16,
  _download_data_id       :: Maybe Word16,
  _logo_char              :: Maybe String
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  logo_transmission_type = _logo_transmission_type 
  logo_id                = _logo_id
  logo_version           = _logo_version
  download_data_id       = _download_data_id
  logo_char              = _logo_char
