module Descriptor.TimeShiftedService where
import Descriptor.Common(Base,HasReferenceServiceID)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasReferenceServiceID a) => TimeShiftedService a where
--  reference_service_id :: a -> Word16
