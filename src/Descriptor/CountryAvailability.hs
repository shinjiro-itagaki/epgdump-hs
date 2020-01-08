module Descriptor.CountryAvailability where
import Descriptor.Common(Base,CountryCode)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a) => CountryAvailability a where
  country_available_flag :: a -> Bool
--  reserved_future_use :: a -> Word8
  country_codes :: a -> [CountryCode]
