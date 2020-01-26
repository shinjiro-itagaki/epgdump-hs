{-# LANGUAGE FlexibleInstances #-}
module Utils.ToWord64 where
import Common(ByteString)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import qualified Data.ByteString.Lazy as BS
import Data.Word(Word64, Word32, Word16, Word8)

class Class a where
  toWord64 :: a -> Word64

instance Class Word8 where
  toWord64 = fromInteger . toInteger

instance Class Word16 where
  toWord64 = fromInteger . toInteger

instance Class Word32 where
  toWord64 = fromInteger . toInteger

instance Class [Word8] where
  toWord64 = Prelude.foldl (\rtn w8 -> (rtn `shiftL` 8) .|. (toWord64 w8) ) (0 :: Word64)

instance Class ByteString where
  toWord64 = toWord64 . BS.unpack

