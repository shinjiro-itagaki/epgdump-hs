module Schedule where
import Data.Word(Word64, Word32, Word16, Word8)

class Class a where
  start_time :: a -> Word64
  duration   :: a -> Word32

data Data = MkData {
  _start_time :: Word64,
  _duration   :: Word32
  }

instance Class Data where
  start_time = _start_time
  duration   = _duration
