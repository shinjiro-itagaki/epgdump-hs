module Descriptor.EmergencyInformation where
import Descriptor.Common(Base,HasServiceID,AreaCode)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (HasServiceID a) => EmergencyInformationItem a where
--  service_id :: a -> Word16
  start_end_flag :: a -> Bool
  signal_level :: a -> Bool
-- reserved_future_use :: a -> Word8
  area_code_length :: a -> Word8
  area_codes :: a -> [AreaCode]
  
class Base a => EmergencyInformation a where

