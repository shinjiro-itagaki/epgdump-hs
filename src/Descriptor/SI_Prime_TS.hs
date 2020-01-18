module Descriptor.SI_Prime_TS (
  Class(..)
  ,Data
  ) where
import Descriptor.SI_Parameter(TableDescription)
import qualified Descriptor.SI_Parameter as SI_Parameter
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header

class (Base.Class a, SI_Parameter.Class a) => Class a where
--  parameter_version :: a -> Word8
--  update_time :: a -> Word16
  si_prime_ts_network_id       :: a -> Word16
  si_prime_transport_stream_id :: a -> Word16
--  table_descriptions :: [SI_TableDescription]

data Data = MkData {
  _header                       :: Header.Data,
  _parameter_version            :: Word8,
  _update_time                  :: Word16,
  _si_prime_ts_network_id       :: Word16,
  _si_prime_transport_stream_id :: Word16,
  _table_descriptions           :: [TableDescription]
  }

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance SI_Parameter.Class Data where
  parameter_version  = _parameter_version
  update_time        = _update_time
  table_descriptions = _table_descriptions  

instance Class Data where
  si_prime_ts_network_id       = _si_prime_ts_network_id
  si_prime_transport_stream_id = _si_prime_transport_stream_id

