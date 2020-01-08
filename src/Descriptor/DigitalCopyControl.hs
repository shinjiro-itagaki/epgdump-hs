module Descriptor.DigitalCopyControl (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),TOS(..),HasOriginalNetworkID(..),HasPrivateDataBytes(..),HasServiceID(..),Descriptor(..),HasUserDefined(..))
import qualified Descriptor.ComponentControl as CC
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, CC.Class a) => Class a where
--  digital_recording_control_data :: a -> Word8
--  maximum_bitrate_flag           :: a -> Bool
  component_control_flag         :: a -> Bool
--  user_defined                   :: a -> Word8
--  maximum_bitrate                :: a -> Maybe Word8
  component_control_length       :: a -> Maybe Word8
  component_controls             :: a -> Maybe [CC.Data]

data Data = MkData {
  _descriptor_tag                 :: Word8,
  _descriptor_length              :: Word8,
  _digital_recording_control_data :: Word8,
  _maximum_bitrate_flag           :: Bool,
  _component_control_flag         :: Bool,
  _user_defined                   :: Word8,
  _maximum_bitrate                :: Maybe Word8,
  _component_control_length       :: Maybe Word8,
  _component_controls             :: Maybe [CC.Data]
}

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance CC.Class Data where
  digital_recording_control_data = _digital_recording_control_data
  maximum_bitrate_flag           = _maximum_bitrate_flag
  maximum_bitrate                = _maximum_bitrate
  
instance HasUserDefined Data where
  user_defined = _user_defined

instance Class Data where  
  component_control_flag   = _component_control_flag
  component_control_length = _component_control_length
  component_controls       = _component_controls
