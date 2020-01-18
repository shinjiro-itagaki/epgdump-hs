module Descriptor.AVC_Video (
  Class(..)
  ,Data
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.CountryCode as CountryCode
import qualified Descriptor.LangCode as LangCode
import Data.Vector(Vector,empty,toList,snoc)

class Base.Class a => Class a where
  profile_idc              :: a -> Word8
  constraint_set0_flag     :: a -> Bool
  constraint_set1_flag     :: a -> Bool  
  constraint_set2_flag     :: a -> Bool
  avc_compatible_flags     :: a -> Word8
  level_idc                :: a -> Word8
  avc_still_present        :: a -> Bool
  avc_24_hour_picture_flag :: a -> Bool
  reserved                 :: a -> Word8

data Data = MkData {
  _header                   :: Header.Data,
  _profile_idc              :: Word8,
  _constraint_set0_flag     :: Bool,
  _constraint_set1_flag     :: Bool,
  _constraint_set2_flag     :: Bool,
  _avc_compatible_flags     :: Word8,
  _level_idc                :: Word8,
  _avc_still_present        :: Bool,
  _avc_24_hour_picture_flag :: Bool,
  _reserved                 :: Word8
  }

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  profile_idc              = _profile_idc
  constraint_set0_flag     = _constraint_set0_flag
  constraint_set1_flag     = _constraint_set1_flag
  constraint_set2_flag     = _constraint_set2_flag
  avc_compatible_flags     = _avc_compatible_flags
  level_idc                = _level_idc
  avc_still_present        = _avc_still_present
  avc_24_hour_picture_flag = _avc_24_hour_picture_flag
  reserved                 = _reserved
