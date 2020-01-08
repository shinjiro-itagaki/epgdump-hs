module Descriptor.SI_Prime_TS where
import Descriptor.Common(Base)
import Descriptor.SI_Parameter(SI_Parameter)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

  
class (Base a, SI_Parameter a) => SI_Prime_TS a where
--  parameter_version :: a -> Word8
--  update_time :: a -> Word16
  si_prime_ts_network_id :: a -> Word16
  si_prime_transport_stream_id :: a -> Word16
--  table_descriptions :: [SI_TableDescription]
