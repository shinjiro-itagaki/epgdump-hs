module Descriptor.ParentalRating where
import Descriptor.Common(Base,CountryCode,HasCountryCode(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

data ParentalRatingItem = MkParentalRatingItemData { 
  _country_code :: CountryCode,
  rating :: Word8
  }

class Base a => ParentalRating a where
  ratings :: a -> [ParentalRatingItem]

instance HasCountryCode ParentalRatingItem where
  country_code = _country_code
