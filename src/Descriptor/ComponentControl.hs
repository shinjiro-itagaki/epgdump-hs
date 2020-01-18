module Descriptor.ComponentControl (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.CountryCode as CountryCode
import Data.Vector(Vector,empty,toList,snoc)

class (Base.Class a) => Class a where
  component_tag                  :: a -> Word8
  digital_recording_control_data :: a -> Word8
  maximum_bitrate_flag           :: a -> Bool
  reserved_future_use            :: a -> Bool
  user_defined                   :: a -> Word8
  maximum_bitrate                :: a -> Maybe Word8

data Data = MkData {
  _header                         :: Header.Data,
  _component_tag                  :: Word8,
  _digital_recording_control_data :: Word8,
  _maximum_bitrate_flag           :: Bool,
  _reserved_future_use            :: Bool,
  _user_defined                   :: Word8,
  _maximum_bitrate                :: Maybe Word8
  }

instance Base.Class Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  component_tag                  = _component_tag  
  digital_recording_control_data = _digital_recording_control_data
  maximum_bitrate_flag           = _maximum_bitrate_flag
  reserved_future_use            = _reserved_future_use
  user_defined                   = _user_defined  
  maximum_bitrate                = _maximum_bitrate
