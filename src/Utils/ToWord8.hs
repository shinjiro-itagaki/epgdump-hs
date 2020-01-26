{-# LANGUAGE FlexibleInstances #-}
module Utils.ToWord8 where
import Common(ByteString)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import qualified Data.ByteString.Lazy as BS
import Data.Word(Word64, Word32, Word16, Word8)

class Class a where
  toWord8 :: a -> Word8

instance Class Word8 where
  toWord8 x = x

instance Class Word16 where
  toWord8 x
    | x > 0xFF = 0xFF
    | otherwise = fromInteger $ toInteger x

instance Class Word32 where
  toWord8 x
    | x > 0xFF = 0xFF
    | otherwise = fromInteger $ toInteger x

instance Class Word64 where
  toWord8 x
    | x > 0xFF = 0xFF
    | otherwise = fromInteger $ toInteger x

instance Class [Word8] where
  toWord8 xs = case xs of
    [] -> 0
    otherwise -> Prelude.last xs

instance Class ByteString where
  toWord8 = BS.last
