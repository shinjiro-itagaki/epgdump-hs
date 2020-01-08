module Descriptor.EmergencyInformation (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),HasServiceID(..),AreaCode,Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)
  
class Base a => Class a where

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)
  
instance Class Data where
  
  
class (HasServiceID a) => ItemClass a where
--  service_id :: a -> Word16
  start_end_flag :: a -> Bool
  signal_level :: a -> Bool
-- reserved_future_use :: a -> Word8
  area_code_length :: a -> Word8
  area_codes :: a -> [AreaCode]

data ItemData = MkItemData {
  _service_id          :: Word16,
  _start_end_flag      :: Bool,
  _signal_level        :: Bool,
--  _reserved_future_use :: Word8,
  _area_code_length    :: Word8,
  _area_codes          :: [AreaCode]
  }

instance HasServiceID ItemData where
  service_id = _service_id

instance ItemClass ItemData where
  start_end_flag   = _start_end_flag
  signal_level     = _signal_level
  area_code_length = _area_code_length
  area_codes       = _area_codes
