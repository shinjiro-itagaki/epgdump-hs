-- 6.2.6
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
import qualified Parser.Result as Result

data Polalization = LinearHorizontal | LinearVertical | CircularLeft | CircularRight deriving (Eq,Show)

data Modulation = M_NotDefined
                | QPSK
                | M_ISDB_S -- ISDB-S system (refer to TMCC signal)
                | M_BS_2600MHz -- 2.6GHz band digital satellite sound broadcasting transmission system (refer to pilot channel)
                | M_CS_ANarrow -- Advanced narrow-band CS digital broadcasting system (refer to PLHEADER and BBHEADER)
                | M_ReservedForFutureUse
                deriving (Eq,Show)

data FEC = FEC_NotDefined
         | Conv_1_2 -- 1/2 conv. code rate
         | Conv_2_3
         | Conv_3_4
         | Conv_5_6
         | Conv_7_8
         | FEC_ISDB_S -- ISDB-S system (refer to TMCC signal)
         | FEC_BS_2600MHz -- 2.6GHz band digital satellite sound broadcasting transmission system (refer to pilot channel)
         | FEC_CS_ANarrow -- Advanced narrow-band CS digital broadcasting system (refer to PLHEADER and BBHEADER)
         | NoConv
         | FEC_ReservedForFutureUse
         deriving (Eq,Show)

toModulation :: Word8 -> Modulation
toModulation x =
  let b3 = (x .&. 0x08) /= 0
      b2 = (x .&. 0x04) /= 0
      b1 = (x .&. 0x02) /= 0
      b0 = (x .&. 0x01) /= 0      
  in case (b3,b2,b1,b0) of
       (False,False,False,False) -> M_NotDefined
       (False,False,False, True) -> QPSK
       (True ,False,False,False) -> M_ISDB_S
       (True ,False,False, True) -> M_BS_2600MHz
       (True ,False,True ,False) -> M_CS_ANarrow
       _                         -> M_ReservedForFutureUse

toPolalization :: Word8 -> Polalization 
toPolalization x =
  let l = (x .&. 0x02) /= 0
      r = (x .&. 0x01) /= 0
  in case (l,r) of
       (False,False) -> LinearHorizontal
       (False, True) -> LinearVertical
       (True, False) -> CircularLeft
       (True,  True) -> CircularRight
       
toFEC :: Word8 -> FEC
toFEC  0 = FEC_NotDefined
toFEC  1 = Conv_1_2 -- 1/2 conv. code rate
toFEC  2 = Conv_2_3
toFEC  3 = Conv_3_4
toFEC  4 = Conv_5_6
toFEC  5 = Conv_7_8
toFEC  8 = FEC_ISDB_S -- ISDB-S system (refer to TMCC signal)
toFEC  9 = FEC_BS_2600MHz -- 2.6GHz band digital satellite sound broadcasting transmission system (refer to pilot channel)
toFEC 10 = FEC_CS_ANarrow -- Advanced narrow-band CS digital broadcasting system (refer to PLHEADER and BBHEADER)
toFEC 15 = NoConv
toFEC _  = FEC_ReservedForFutureUse

class Base.Class a => Class a where
  frequency        :: a -> Word32 -- 32
  orbital_position :: a -> Word16 -- 16
  west_east_flag   :: a -> Bool -- 1
  polarization     :: a -> Polalization -- 2
  modulation       :: a -> Modulation -- 5
  system_rate      :: a -> Word32 --28
  fec_inner        :: a -> FEC -- 4

data Data = MkData {
  _header            :: Header.Data,
  _frequency         :: Word32,
  _orbital_position  :: Word16,
  _west_east_flag    :: Bool,
  _polarization      :: Polalization,
  _modulation        :: Modulation,
  _system_rate       :: Word32,
  _fec_inner         :: FEC
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
        modulation'     = toModulation $ (w8 .&. 0x1F) `shiftR` 5 -- 0b00011111
        system_rate'    = (w32 .&. 0xFFFFFFF0) `shiftR` 4 -- 0xFFFFFFF0
        fec_inner'      = toFEC $ fromInteger $ toInteger $ (w32 .&. 0x0000000F) `shiftR` 0 -- 0x0000000F
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
    in Result.Parsed d


instance Class Data where
  frequency        = _frequency
  orbital_position = _orbital_position
  west_east_flag   = _west_east_flag
  polarization     = _polarization
  modulation       = _modulation
  system_rate      = _system_rate
  fec_inner        = _fec_inner
