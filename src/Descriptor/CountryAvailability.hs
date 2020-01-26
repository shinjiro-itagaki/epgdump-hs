-- 6.2.5
module Descriptor.CountryAvailability (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Utils.CountryCode as CountryCode
import Data.Vector(Vector,empty,toList,snoc)
import Utils.FromByteString(fromByteString,fromByteStringWithRest,fromByteStringWithRestM)
import Data.Bits((.&.))

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
  fromByteStringAfterHeader h bs =
    -- gather :: BytesLen -> (ByteString -> (Maybe b, ByteString)) -> (a -> b -> a) -> ByteString -> a -> (a,ByteString)              
    let (w8,bs1) = fromByteStringWithRest bs
        country_available_flag = (w8 .&. 0x80) /= 0 :: Bool
        reserved_future_use    = (w8 .&. 0x7F)
        (country_codes,bs2)    = Base.gather (Header.descriptor_length h) fromByteStringWithRestM snoc bs1 empty
        d = MkData {
          _header                 = h,
          _country_available_flag = country_available_flag,
          _reserved_future_use    = reserved_future_use,
          _country_codes          = country_codes
          }
    in (Just d, bs2)

instance Class Data where
  country_available_flag = _country_available_flag
  reserved_future_use    = _reserved_future_use
  country_codes          = toList . _country_codes
