module Descriptor.SI_Prime_TS (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Descriptor.SI_Parameter(TableDescription)
import qualified Descriptor.SI_Parameter as SI_Parameter
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, SI_Parameter.Class a) => Class a where
--  parameter_version :: a -> Word8
--  update_time :: a -> Word16
  si_prime_ts_network_id       :: a -> Word16
  si_prime_transport_stream_id :: a -> Word16
--  table_descriptions :: [SI_TableDescription]

data Data = MkData {
  _descriptor_tag               :: Word8,
  _descriptor_length            :: Word8,
  _parameter_version            :: Word8,
  _update_time                  :: Word16,
  _si_prime_ts_network_id       :: Word16,
  _si_prime_transport_stream_id :: Word16,
  _table_descriptions           :: [TableDescription]
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance SI_Parameter.Class Data where
  parameter_version  = _parameter_version
  update_time        = _update_time
  table_descriptions = _table_descriptions  

instance Class Data where
  si_prime_ts_network_id       = _si_prime_ts_network_id
  si_prime_transport_stream_id = _si_prime_transport_stream_id

