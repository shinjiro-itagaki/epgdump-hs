module SITables.NIT where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(CommonHeader(..) ,CommonHeader2(..))
import Descriptor()

class (CommonHeader a ,CommonHeader2 a) => NIT a where
  network_id :: a -> Word16
  network_description_length :: a -> Word16
  transport_stream_loop_length :: a -> Word16
  
