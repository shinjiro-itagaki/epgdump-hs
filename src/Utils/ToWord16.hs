{-# LANGUAGE FlexibleInstances #-}
module Utils.ToWord16 where
import Common(ByteString)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import qualified Data.ByteString.Lazy as BS
import Data.Word(Word64, Word32, Word16, Word8)

class Class a where
  toWord16 :: a -> Word16

instance Class Word8 where
  toWord16 = fromInteger . toInteger

instance Class [Word8] where
  toWord16 = Prelude.foldl (\rtn w8 -> (rtn `shiftL` 8) .|. (toWord16 w8) ) (0 :: Word16)

instance Class ByteString where
  toWord16 = toWord16 . BS.unpack
