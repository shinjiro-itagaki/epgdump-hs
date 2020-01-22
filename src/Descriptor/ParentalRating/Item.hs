module Descriptor.ParentalRating.Item (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.CountryCode as CountryCode
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)

class Class a where
  country_code :: a -> CountryCode.Data
  rating       :: a -> Word8  

data Data = MkData { 
  _country_code :: CountryCode.Data,
  _rating       :: Word8
  } deriving (Show)

instance Class Data where
  country_code = _country_code  
  rating       = _rating 
