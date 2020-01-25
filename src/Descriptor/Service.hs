module Descriptor.Service (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header

class (Base.Class a) => Class a where
  service_type :: a -> Word8
  service_provider_name_length :: a -> Word8
  service_provider_name        :: a -> String
  service_name_length          :: a -> Word8
  service_name                 :: a -> String

data Data = MkData {
  _header                       :: Header.Data,
  _service_type                 :: Word8,
  _service_provider_name_length :: Word8,
  _service_provider_name        :: String,
  _service_name_length          :: Word8,
  _service_name                 :: String
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  service_type                  = _service_type  
  service_provider_name_length  = _service_provider_name_length
  service_provider_name         = _service_provider_name
  service_name_length           = _service_name_length
  service_name                  = _service_name
