module Descriptor.LogoTransmission where
import Descriptor.Common(Base)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => LogoTransmission a where
  logo_transmission_type :: a -> Word8
  logo_id :: a -> Maybe Word16
  logo_version :: a -> Maybe Word16
  download_data_id :: a -> Maybe Word16
  logo_char :: a -> Maybe String
