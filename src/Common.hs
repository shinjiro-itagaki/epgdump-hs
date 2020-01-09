module Common where
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString(ByteString)
class HasOriginalNetworkID a where
  original_network_id :: a -> Word16

