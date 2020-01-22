module Descriptor.CarouselCompatibleComposite.SubDescriptor.Expire (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)

import qualified Descriptor.Header as Header
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Base as Base

class (Base.Class a) => Class a where
  time_mode           :: a -> Word8
  _MJD_JST_time       :: a -> Maybe Word64
  reserved_future_use :: a -> Maybe Word8
  passed_seconds      :: a -> Maybe Word32

data Data = MkData {
  _header              :: Header.Data,  
  _time_mode           :: Word8,
  __MJD_JST_time       :: Maybe Word64,
  _reserved_future_use :: Maybe Word8,
  _passed_seconds      :: Maybe Word32
  } deriving (Show)

instance Header.Class Data where
  header = _header
  setHeader x h = x {_header = h}

instance Base.Class Data where

instance Class Data where
  time_mode           = _time_mode
  _MJD_JST_time       = __MJD_JST_time
  passed_seconds      = _passed_seconds
  reserved_future_use = _reserved_future_use

