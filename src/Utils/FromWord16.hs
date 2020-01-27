{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.FromWord16 where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Char(chr)
import Data.Bits((.&.),(.|.),shiftL,shiftR,finiteBitSize)

class Class a where
  fromWord16 :: Word16 -> a
  
instance Class Word8 where
  fromWord16 = fromInteger . toInteger . (.&. 0xFF)

instance Class Bool where
  fromWord16 = (/= 0)
  
instance Class (Bool,Bool) where
  fromWord16 x = ((x .&. 0x02) /= 0, (x .&. 0x01) /= 0)
  
instance Class (Bool,Bool,Bool) where
  fromWord16 x = ((x .&. 0x04) /= 0, (x .&. 0x02) /= 0, (x .&. 0x01) /= 0)

instance Class (Bool,Bool,Bool,Bool) where
  fromWord16 x = ((x .&. 0x08) /= 0, (x .&. 0x04) /= 0, (x .&. 0x02) /= 0, (x .&. 0x01) /= 0)

instance Class (Bool,Bool,Bool,Bool,Bool) where
  fromWord16 x = ((x .&. 0x10) /= 0, (x .&. 0x08) /= 0, (x .&. 0x04) /= 0, (x .&. 0x02) /= 0, (x .&. 0x01) /= 0)

instance Class (Bool,Bool,Bool,Bool,Bool,Bool) where
  fromWord16 x = ((x .&. 0x20) /= 0, (x .&. 0x10) /= 0, (x .&. 0x08) /= 0, (x .&. 0x04) /= 0, (x .&. 0x02) /= 0, (x .&. 0x01) /= 0)

instance Class (Bool,Bool,Bool,Bool,Bool,Bool,Bool) where
  fromWord16 x = ((x .&. 0x40) /= 0,(x .&. 0x20) /= 0, (x .&. 0x10) /= 0, (x .&. 0x08) /= 0, (x .&. 0x04) /= 0, (x .&. 0x02) /= 0, (x .&. 0x01) /= 0)

instance Class (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool) where
  fromWord16 x = ((x .&. 0x80) /= 0, (x .&. 0x40) /= 0,(x .&. 0x20) /= 0, (x .&. 0x10) /= 0, (x .&. 0x08) /= 0, (x .&. 0x04) /= 0, (x .&. 0x02) /= 0, (x .&. 0x01) /= 0)


