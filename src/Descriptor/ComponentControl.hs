module Descriptor.ComponentControl where
import Descriptor.Common(Base,HasUserDefined(..),HasComponentTag(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (HasUserDefined a) => ComponentControl a where
  digital_recording_control_data :: a -> Word8
  maximum_bitrate_flag           :: a -> Bool
--  reserved_future_use            :: a -> Bool
--  user_defined                   :: a -> Word8
  maximum_bitrate                :: a -> Maybe Word8

data ComponentControlData = MkComponentControl {
  _component_tag                  :: Word8,
  _digital_recording_control_data :: Word8,
  _maximum_bitrate_flag           :: Bool,
  _user_defined                   :: Word8,
  _maximum_bitrate                :: Maybe Word8  
  }

instance ComponentControl ComponentControlData where
  digital_recording_control_data = _digital_recording_control_data
  maximum_bitrate_flag           = _maximum_bitrate_flag
  maximum_bitrate                = _maximum_bitrate
  
instance HasUserDefined ComponentControlData where
  user_defined = _user_defined

instance HasComponentTag ComponentControlData where
  component_tag = _component_tag
