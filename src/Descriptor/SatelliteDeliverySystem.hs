module Descriptor.SatelliteDeliverySystem (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  frequency        :: a -> Word32
  orbital_position :: a -> Word16
  west_east_flag   :: a -> Bool
  polarization     :: a -> Word8
  modulation       :: a -> Word8
  system_rate      :: a -> Word32
  fec_inner        :: a -> Word8

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8,
  _frequency         :: Word32,
  _orbital_position  :: Word16,
  _west_east_flag    :: Bool,
  _polarization      :: Word8,
  _modulation        :: Word8,
  _system_rate       :: Word32,
  _fec_inner         :: Word8
  }
  
instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  frequency        = _frequency
  orbital_position = _orbital_position
  west_east_flag   = _west_east_flag
  polarization     = _polarization
  modulation       = _modulation
  system_rate      = _system_rate
  fec_inner        = _fec_inner
