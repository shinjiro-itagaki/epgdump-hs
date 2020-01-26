module Descriptor.AVC_Timing_HRD (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Utils.LangCode as LangCode
import Data.Vector(Vector,empty,toList,snoc)

class Base.Class a => Class a where
  hrd_management_valid_flag          :: a -> Bool
  reserved1                          :: a -> Word8
  picture_and_timing_info_present    :: a -> Bool
  _90kHz_flag                        :: a -> Maybe Bool
  reserved2                          :: a -> Word8
  _N                                 :: a -> Maybe Word32
  _K                                 :: a -> Maybe Word32
  num_units_in_tick                  :: a -> Word32
  fixed_farme_rate_flag              :: a -> Bool
  temporal_poc_flag                  :: a -> Bool
  picture_to_display_conversion_flag :: a -> Bool
  reserved3                          :: a -> Word8

data Data = MkData {
  _header                             :: Header.Data,
  _hrd_management_valid_flag          :: Bool,
  _reserved1                          :: Word8,
  _picture_and_timing_info_present    :: Bool,
  __90kHz_flag                        :: Maybe Bool,
  _reserved2                          :: Word8,
  __N                                 :: Maybe Word32,
  __K                                 :: Maybe Word32,
  _num_units_in_tick                  :: Word32,
  _fixed_farme_rate_flag              :: Bool,
  _temporal_poc_flag                  :: Bool,
  _picture_to_display_conversion_flag :: Bool,
  _reserved3                          :: Word8
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where

instance Class Data where
  hrd_management_valid_flag          = _hrd_management_valid_flag
  reserved1                          = _reserved1
  picture_and_timing_info_present    = _picture_and_timing_info_present
  _90kHz_flag                        = __90kHz_flag
  reserved2                          = _reserved2  
  _N                                 = __N
  _K                                 = __K
  num_units_in_tick                  = _num_units_in_tick
  fixed_farme_rate_flag              = _fixed_farme_rate_flag
  temporal_poc_flag                  = _temporal_poc_flag
  picture_to_display_conversion_flag = _picture_to_display_conversion_flag
  reserved3                          = _reserved3
