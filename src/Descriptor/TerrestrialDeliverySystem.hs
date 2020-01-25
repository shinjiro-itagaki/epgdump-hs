module Descriptor.TerrestrialDeliverySystem (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.AreaCode as AreaCode
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header


class Base.Class a => Class a where
  area_code         :: a -> AreaCode.Data
  guard_interval    :: a -> Word8
  transmission_mode :: a -> Word8
  frequencies       :: a -> [Word16]

data Data = MkData {
  _header            :: Header.Data,
  _area_code         :: AreaCode.Data,
  _guard_interval    :: Word8,
  _transmission_mode :: Word8,
  _frequencies       :: [Word16]
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where

instance Class Data where
  area_code         = _area_code
  guard_interval    = _guard_interval
  transmission_mode = _transmission_mode
  frequencies       = _frequencies
