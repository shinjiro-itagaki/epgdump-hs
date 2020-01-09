module Descriptor.ExtendedBroadcaster (
  Class(..)
  ,Data
  ) where
import Common(HasOriginalNetworkID(..))
import Descriptor.Common(Base(..),HasMaybePrivateDataBytes(..),HasPrivateDataBytes(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasMaybePrivateDataBytes a) => Class a where
  broadcaster_type :: a -> Word8
-- reserved_future_use
  maybe_broadcaster :: a -> Maybe Broadcaster

data Data = MkData {
  _descriptor_tag        :: Word8,
  _descriptor_length     :: Word8,
  _broadcaster_type      :: Word8,
  _maybe_broadcaster     :: Maybe Broadcaster,
  _maybe_private_data_bytes :: Maybe [Word8]
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance HasMaybePrivateDataBytes Data where
  maybe_private_data_bytes = _maybe_private_data_bytes 

instance Class Data where  
  broadcaster_type  = _broadcaster_type
  maybe_broadcaster = _maybe_broadcaster

-- class Base a => SystemControl a where
class (HasPrivateDataBytes a) => BroadcasterCommon a where
  number_of_broadcaster_id_loop :: a -> Word8
  info :: a -> [BroadcasterInfo]
--  private_data_bytes :: a -> [Word8]

data Broadcaster =
  MkTV {
  terrestrial_broadcaster_id :: Word16,
  number_of_affiliation_id_loop :: Word8,
  _tv_number_of_broadcaster_id_loop :: Word8,
  affiliation_ids :: [Word8],
  _tv_info :: [BroadcasterInfo],
  _tv_private_data_bytes :: [Word8]
  } |
  MkSd {
  terrestrial_sound_broadcaster_id :: Word16,
  number_of_sound_broadcast_affiliation_id_loop :: Word8,
  _sd_number_of_broadcaster_id_loop :: Word8,
  sound_broadcast_affiliation_ids :: [Word8],
  _sd_info :: [BroadcasterInfo],
  _sd_private_data_bytes :: [Word8]
  }


data BroadcasterInfo = MkBroadcasterInfo {
  broadcaster_id :: Word8,
  _broadcaster_original_network_id :: Word16
  }  


instance HasOriginalNetworkID BroadcasterInfo where
  original_network_id = _broadcaster_original_network_id



instance HasPrivateDataBytes Broadcaster where
  private_data_bytes (MkTV {_tv_private_data_bytes = x}) = x
  private_data_bytes (MkSd {_sd_private_data_bytes = x}) = x

instance BroadcasterCommon Broadcaster where
  number_of_broadcaster_id_loop (MkTV {_tv_number_of_broadcaster_id_loop = x}) = x
  number_of_broadcaster_id_loop (MkSd {_sd_number_of_broadcaster_id_loop = x}) = x
  info (MkTV {_tv_info = x}) = x
  info (MkSd {_sd_info = x}) = x

