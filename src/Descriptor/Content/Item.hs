module Descriptor.Content.Item (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  

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
  
