module Descriptor.ComponentControl (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),HasUserDefined(..),HasComponentTag(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (HasUserDefined a) => Class a where
  digital_recording_control_data :: a -> Word8
  maximum_bitrate_flag           :: a -> Bool
--  reserved_future_use            :: a -> Bool
--  user_defined                   :: a -> Word8
  maximum_bitrate                :: a -> Maybe Word8

data Data = MkData {
  _descriptor_tag                 :: Word8,
  _descriptor_length              :: Word8,  
  _component_tag                  :: Word8,
  _digital_recording_control_data :: Word8,
  _maximum_bitrate_flag           :: Bool,
  _user_defined                   :: Word8,
  _maximum_bitrate                :: Maybe Word8  
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  digital_recording_control_data = _digital_recording_control_data
  maximum_bitrate_flag           = _maximum_bitrate_flag
  maximum_bitrate                = _maximum_bitrate
  
instance HasUserDefined Data where
  user_defined = _user_defined

instance HasComponentTag Data where
  component_tag = _component_tag
