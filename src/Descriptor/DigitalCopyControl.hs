module Descriptor.DigitalCopyControl where
import Descriptor.Common(Base)
import Descriptor.ComponentControl(ComponentControl,ComponentControlData)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, ComponentControl a) => DigitalCopyControl a where
--  digital_recording_control_data :: a -> Word8
--  maximum_bitrate_flag           :: a -> Bool
  component_control_flag         :: a -> Bool
--  user_defined                   :: a -> Word8
--  maximum_bitrate                :: a -> Maybe Word8
  component_control_length       :: a -> Maybe Word8
  component_controls             :: a -> Maybe [ComponentControlData]

