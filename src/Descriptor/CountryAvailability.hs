module Descriptor.CountryAvailability (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.CountryCode as CountryCode
import Data.Vector(Vector,empty,toList,snoc)

class (Base.Class a) => Class a where
  country_available_flag :: a -> Bool
  reserved_future_use    :: a -> Word8
  country_codes          :: a -> [CountryCode.Data]

data Data = MkData {
  _header                 :: Header.Data,
  _country_available_flag :: Bool,
  _reserved_future_use    :: Word8,
  _country_codes          :: Vector CountryCode.Data
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  country_available_flag = _country_available_flag
  reserved_future_use    = _reserved_future_use
  country_codes          = toList . _country_codes
