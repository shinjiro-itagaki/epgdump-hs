module Descriptor.CountryAvailability (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),CountryCode,Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a) => Class a where
  country_available_flag :: a -> Bool
--  reserved_future_use :: a -> Word8
  country_codes :: a -> [CountryCode]

data Data = MkData {
  _descriptor_tag         :: Word8,
  _descriptor_length      :: Word8,
  _country_available_flag :: Bool,
--  reserved_future_use :: a -> Word8
  _country_codes          :: [CountryCode]
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  country_available_flag = _country_available_flag
  country_codes          = _country_codes
