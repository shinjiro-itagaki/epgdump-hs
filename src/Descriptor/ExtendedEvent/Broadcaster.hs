module Descriptor.ExtendedEvent.Broadcaster (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)

class Class a where
  terrestrial_broadcaster_id    :: a -> Word16
  number_of_affiliation_id_loop :: a -> Word8 
  number_of_broadcaster_id_loop :: a -> Word8
  affiliation_ids               :: a -> [Word8]
  info                          :: a -> [Info]
  private_data_bytes            :: a -> ByteString

data Data =
  IsTV TV
  | IsSD SD
  | IsNone None
  deriving (Show)

data TV = MkTV {
  _terrestrial_broadcaster_id       :: Word16,
  _number_of_affiliation_id_loop    :: Word8,
  _tv_number_of_broadcaster_id_loop :: Word8,
  _affiliation_ids                  :: [Word8],
  _tv_info                          :: [Info],
  _tv_private_data_bytes            :: ByteString
  } deriving (Show)

data SD = MkSD {
  _terrestrial_sound_broadcaster_id              :: Word16,
  _number_of_sound_broadcast_affiliation_id_loop :: Word8,
  _sd_number_of_broadcaster_id_loop              :: Word8,
  _sound_broadcast_affiliation_ids               :: [Word8],
  _sd_info                                       :: [Info],
  _sd_private_data_bytes                         :: ByteString
  } deriving (Show)
  
data None = MkNone {
  _reserved_future_use :: [Word8]
  } deriving (Show)


data Info = MkInfo {
  _broadcaster_id      :: Word8,
  _original_network_id :: Word16
  } deriving (Show)

  
instance Class TV where
  terrestrial_broadcaster_id    = _terrestrial_broadcaster_id
  number_of_affiliation_id_loop = _number_of_affiliation_id_loop
  number_of_broadcaster_id_loop = _tv_number_of_broadcaster_id_loop
  affiliation_ids               = _affiliation_ids
  info                          = _tv_info
  private_data_bytes            = _tv_private_data_bytes
  
instance Class SD where
  terrestrial_broadcaster_id    = _terrestrial_sound_broadcaster_id
  number_of_affiliation_id_loop = _number_of_sound_broadcast_affiliation_id_loop
  number_of_broadcaster_id_loop = _sd_number_of_broadcaster_id_loop
  affiliation_ids               = _sound_broadcast_affiliation_ids
  info                          = _sd_info
  private_data_bytes            = _sd_private_data_bytes
  
  

  
