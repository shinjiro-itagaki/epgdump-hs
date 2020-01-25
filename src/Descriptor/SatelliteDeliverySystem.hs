module Descriptor.SatelliteDeliverySystem (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Utils.FromByteString(fromByteString,fromByteStringWithRest)
import Data.Bits((.&.),testBit,shiftR)

data Polalization = LinearHorizontal | LinearVertical | CircularLeft | CircularRight deriving (Eq,Show)

toPolalization :: Word8 -> Polalization 
toPolalization x =
  let l = (x .&. 0x02) /= 0
      r = (x .&. 0x01) /= 0
  in case (l,r) of
       (False,False) -> LinearHorizontal
       (False, True) -> LinearVertical
       (True, False) -> CircularLeft
       (True,  True) -> CircularRight


class Base.Class a => Class a where
  frequency        :: a -> Word32 -- 32
  orbital_position :: a -> Word16 -- 16
  west_east_flag   :: a -> Bool -- 1
  polarization     :: a -> Polalization -- 2
  modulation       :: a -> Word8 -- 5
  system_rate      :: a -> Word32 --28
  fec_inner        :: a -> Word8 -- 4

data Data = MkData {
  _header            :: Header.Data,
  _frequency         :: Word32,
  _orbital_position  :: Word16,
  _west_east_flag    :: Bool,
  _polarization      :: Polalization,
  _modulation        :: Word8,
  _system_rate       :: Word32,
  _fec_inner         :: Word8
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
  fromByteStringAfterHeader h bs0 =
    let (frequency'        ,bs1) = fromByteStringWithRest bs0
        (orbital_position' ,bs2) = fromByteStringWithRest bs1
        (w8                ,bs3) = fromByteStringWithRest bs2
        (w32               ,bs4) = fromByteStringWithRest bs3
        
        west_east_flag' = (w8 .&. 0x80) > 0        -- 0b10000000
        polarization'   = toPolalization $ (w8 .&. 0x60) `shiftR` 5 -- 0b01100000
        modulation'     = (w8 .&. 0x1F) `shiftR` 5 -- 0b00011111
        system_rate'    = (w32 .&. 0xFFFFFFF0) `shiftR` 4 -- 0xFFFFFFF0
        fec_inner'      = fromInteger $ toInteger $ (w32 .&. 0x0000000F) `shiftR` 0 -- 0x0000000F
        d = MkData {
          _header            = h,
          _frequency         = frequency',
          _orbital_position  = orbital_position',
          _west_east_flag    = west_east_flag',
          _polarization      = polarization',
          _modulation        = modulation',
          _system_rate       = system_rate',
          _fec_inner         = fec_inner'
          }
    in (Just d,bs4)


instance Class Data where
  frequency        = _frequency
  orbital_position = _orbital_position
  west_east_flag   = _west_east_flag
  polarization     = _polarization
  modulation       = _modulation
  system_rate      = _system_rate
  fec_inner        = _fec_inner
