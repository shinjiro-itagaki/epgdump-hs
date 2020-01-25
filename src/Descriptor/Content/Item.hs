module Descriptor.Content.Item (
  Class(..)
  ,Data
  ,mk
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Data.Bits((.&.),shiftR)

class Class a where
  content_nibble_level_1 :: a -> Word8
  content_nibble_level_2 :: a -> Word8
  user_nibble_1 :: a -> Word8
  user_nibble_2 :: a -> Word8  

data Data = MkData {
  _content_nibble_level_1 :: Word8,
  _content_nibble_level_2 :: Word8,
  _user_nibble_1 :: Word8,
  _user_nibble_2 :: Word8
  } deriving (Show)

instance Class Data where
  content_nibble_level_1 = _content_nibble_level_1
  content_nibble_level_2 = _content_nibble_level_2
  user_nibble_1 = _user_nibble_1
  user_nibble_2 = _user_nibble_2

mk :: [Word8] -> Data
mk []         = _mk 0 0
mk (x:[])     = _mk x 0
mk (x:(y:ys)) = _mk x y

_mk :: Word8 -> Word8 -> Data
_mk x y = MkData {
  _content_nibble_level_1 = (`shiftR` 4) $ 0xF0 .&. x,
  _content_nibble_level_2 = (`shiftR` 0) $ 0x0F .&. x,
  _user_nibble_1          = (`shiftR` 4) $ 0xF0 .&. y,
  _user_nibble_2          = (`shiftR` 0) $ 0x0F .&. y
  }
