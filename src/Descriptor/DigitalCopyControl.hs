module Descriptor.DigitalCopyControl (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.CountryCode as CountryCode
import qualified Descriptor.ComponentControl as ComponentControl
import Data.Vector(Vector,empty,toList,snoc)
class (Base.Class a) => Class a where
  digital_recording_control_data :: a -> Word8
  maximum_bitrate_flag           :: a -> Bool
  component_control_flag         :: a -> Bool
  user_defined                   :: a -> Word8
  maximum_bitrate                :: a -> Maybe Word8
  component_control_length       :: a -> Maybe Word8
  component_controls             :: a -> Maybe [ComponentControl.Data]

data Data = MkData {
  _header                         :: Header.Data,
  _digital_recording_control_data :: Word8,
  _maximum_bitrate_flag           :: Bool,
  _component_control_flag         :: Bool,
  _user_defined                   :: Word8,
  _maximum_bitrate                :: Maybe Word8,
  _component_control_length       :: Maybe Word8,
  _component_controls             :: Maybe (Vector ComponentControl.Data)
} deriving (Show)

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  digital_recording_control_data = _digital_recording_control_data
  maximum_bitrate_flag           = _maximum_bitrate_flag
  maximum_bitrate                = _maximum_bitrate
  user_defined                   = _user_defined
  component_control_flag         = _component_control_flag
  component_control_length       = _component_control_length
  component_controls             = (fmap toList) .  _component_controls
