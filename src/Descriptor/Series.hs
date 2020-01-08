module Descriptor.Series (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..),AreaCode)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)
  
class Base a => Class a where
  series_id              :: a -> Word16
  repeat_label           :: a -> Word8
  program_pattern        :: a -> Word8
  expire_date_valid_flag :: a -> Bool
  expire_date            :: a -> Word16
  episode_number         :: a -> Word16
  last_episode_number    :: a -> Word16
  series_name            :: a -> String

data Data = MkData {
  _descriptor_tag         :: Word8,
  _descriptor_length      :: Word8,
  _series_id              :: Word16,
  _repeat_label           :: Word8,
  _program_pattern        :: Word8,
  _expire_date_valid_flag :: Bool,
  _expire_date            :: Word16,
  _episode_number         :: Word16,
  _last_episode_number    :: Word16,
  _series_name            :: String
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  series_id              = _series_id
  repeat_label           = _repeat_label
  program_pattern        = _program_pattern
  expire_date_valid_flag = _expire_date_valid_flag
  expire_date            = _expire_date
  episode_number         = _episode_number
  last_episode_number    = _last_episode_number
  series_name            = _series_name
