{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.FromWord32 where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Char(chr)
import Data.Bits((.&.),(.|.),shiftL,shiftR,finiteBitSize)

class Class a where
  fromWord32 :: Word32 -> a

instance Class Char where
  fromWord32 x = chr $ fromInteger $ toInteger ((fromWord32 x) :: Word8)

instance Class Bool where
  fromWord32 = (/= 0)

instance Class (Bool,Bool) where
  fromWord32 x = ((x .&. 0x02 ) > 0,(x .&. 0x01) > 0)

instance Class Word8 where
  fromWord32 = fromInteger . toInteger . (.&. 0xFF)

instance Class Word16 where
  fromWord32 = fromInteger . toInteger . (.&. 0xFFFF)

instance Class Word32 where
  fromWord32 x = x
  
instance Class Word64 where
  fromWord32 = fromInteger . toInteger
