module Descriptor.LogoTransmission (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)

data LogoTransmission =
  Type1 { -- 0x01
  reserved_future_use1 :: Word8, -- 7
  logo_id              :: Word16, -- 9
  reserved_future_use2 :: Word8, -- 4
  logo_version         :: Word16, -- 12
  download_data_id     :: Word16 -- 16
  }
  | Type2 { -- 0x02
      reserved_future_use1 :: Word8, -- 7
      logo_id              :: Word16 -- 9      
      }
  | Type3 { -- 0x03
      logo_char            :: String  -- [8]
      }
  | TypeExcept { -- others
      reserved_future_uses :: [Word8] -- [8]
      }
  deriving (Show)

class Base.Class a => Class a where
  logo_transmission_type :: a -> Word8
  logo_transmission      :: a -> LogoTransmission

data Data = MkData {
  _header                 :: Header.Data,
  _logo_transmission_type :: Word8,
  _logo_transmission      :: LogoTransmission
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  logo_transmission_type = _logo_transmission_type
  logo_transmission      = _logo_transmission
