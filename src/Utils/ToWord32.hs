{-# LANGUAGE FlexibleInstances #-}
module Utils.ToWord32 where
import Common(ByteString)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import qualified Data.ByteString.Lazy as BS
import Data.Word(Word64, Word32, Word16, Word8)

class Class a where
  toWord32 :: a -> Word32

instance Class Word8 where
  toWord32 = fromInteger . toInteger

instance Class [Word8] where
  toWord32 = Prelude.foldl (\rtn w8 -> (rtn `shiftL` 8) .|. (toWord32 w8) ) (0 :: Word32)

instance Class ByteString where
  toWord32 = toWord32 . BS.unpack


