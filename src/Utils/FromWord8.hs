{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.FromWord8 where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Char(chr)
import Data.Bits((.&.),(.|.),shiftL,shiftR,finiteBitSize)

class Class a where
  fromWord8 :: Word8 -> a

instance Class Char where
  fromWord8 = chr . fromInteger . toInteger 

instance Class Bool where
  fromWord8 = (/= 0)

instance Class (Bool,Bool) where
  fromWord8 x = ((x .&. 0x02) /= 0, (x .&. 0x01) /= 0)
  
instance Class (Bool,Bool,Bool) where
  fromWord8 x = ((x .&. 0x04) /= 0, (x .&. 0x02) /= 0, (x .&. 0x01) /= 0)

instance Class (Bool,Bool,Bool,Bool) where
  fromWord8 x = ((x .&. 0x08) /= 0, (x .&. 0x04) /= 0, (x .&. 0x02) /= 0, (x .&. 0x01) /= 0)

instance Class (Bool,Bool,Bool,Bool,Bool) where
  fromWord8 x = ((x .&. 0x10) /= 0, (x .&. 0x08) /= 0, (x .&. 0x04) /= 0, (x .&. 0x02) /= 0, (x .&. 0x01) /= 0)

instance Class (Bool,Bool,Bool,Bool,Bool,Bool) where
  fromWord8 x = ((x .&. 0x20) /= 0, (x .&. 0x10) /= 0, (x .&. 0x08) /= 0, (x .&. 0x04) /= 0, (x .&. 0x02) /= 0, (x .&. 0x01) /= 0)

instance Class (Bool,Bool,Bool,Bool,Bool,Bool,Bool) where
  fromWord8 x = ((x .&. 0x40) /= 0,(x .&. 0x20) /= 0, (x .&. 0x10) /= 0, (x .&. 0x08) /= 0, (x .&. 0x04) /= 0, (x .&. 0x02) /= 0, (x .&. 0x01) /= 0)

instance Class (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool) where
  fromWord8 x = ((x .&. 0x80) /= 0, (x .&. 0x40) /= 0,(x .&. 0x20) /= 0, (x .&. 0x10) /= 0, (x .&. 0x08) /= 0, (x .&. 0x04) /= 0, (x .&. 0x02) /= 0, (x .&. 0x01) /= 0)


