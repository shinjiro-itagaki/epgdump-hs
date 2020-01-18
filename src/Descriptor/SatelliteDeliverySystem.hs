module Descriptor.SatelliteDeliverySystem (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header


class Base.Class a => Class a where
  frequency        :: a -> Word32
  orbital_position :: a -> Word16
  west_east_flag   :: a -> Bool
  polarization     :: a -> Word8
  modulation       :: a -> Word8
  system_rate      :: a -> Word32
  fec_inner        :: a -> Word8

data Data = MkData {
  _frequency         :: Word32,
  _orbital_position  :: Word16,
  _west_east_flag    :: Bool,
  _polarization      :: Word8,
  _modulation        :: Word8,
  _system_rate       :: Word32,
  _fec_inner         :: Word8
  }
  
instance Base.Class Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  frequency        = _frequency
  orbital_position = _orbital_position
  west_east_flag   = _west_east_flag
  polarization     = _polarization
  modulation       = _modulation
  system_rate      = _system_rate
  fec_inner        = _fec_inner
