module Descriptor.TimeShiftedEvent (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header

class (Base.Class a) => Class a where
  reference_service_id :: a -> Word16
  reference_event_id :: a -> Word16

data Data = MkData {
  _header                 :: Header.Data,
  _reference_service_id   :: Word16,
  _reference_event_id     ::  Word16
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  reference_service_id = _reference_service_id
  reference_event_id = _reference_event_id
