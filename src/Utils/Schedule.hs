module Utils.Schedule where
import Common
import Utils.FromWord64 hiding (Class)
import Utils.FromByteString hiding (Class)
import qualified Utils.FromByteString as FromByteString 

class Class a where
  start_time :: a -> Word64 -- 40
  duration   :: a -> Word32 -- 24

data Data = MkData {
  _start_time :: Word64,
  _duration   :: Word32
  } deriving (Show)

instance Class Data where
  start_time = _start_time
  duration   = _duration

instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    let (w64,rest) = fromByteStringWithRest bs
        x =              (w64 .&. 0xFFFFFFFFFF000000) `shiftR` 24
        y = fromWord64 $ (w64 .&. 0x0000000000FFFFFF)
        d = MkData {
          _start_time = x,
          _duration   = y
          }
    in (d,rest)
