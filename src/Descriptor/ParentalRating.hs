module Descriptor.ParentalRating (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..),CountryCode,HasCountryCode(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  ratings :: a -> [ParentalRatingItem]

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _ratings           :: [ParentalRatingItem]
  }

data ParentalRatingItem = MkParentalRatingItemData { 
  _country_code :: CountryCode,
  rating :: Word8
  }

instance HasCountryCode ParentalRatingItem where
  country_code = _country_code

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  ratings = _ratings
