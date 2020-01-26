{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.FromWord64 where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Char(chr)
import Data.Bits((.&.),(.|.),shiftL,shiftR,finiteBitSize)

class Class a where
  fromWord64 :: Word64 -> a

instance Class Char where
  fromWord64 x = chr $ fromInteger $ toInteger ((fromWord64 x) :: Word8)
  
instance Class Bool where
  fromWord64 = (> 0) . (.&. 0x01)

instance Class (Bool,Bool) where
  fromWord64 x = ((x .&. 0x02 ) > 0,(x .&. 0x01) > 0)

instance Class Word8 where
  fromWord64 = fromInteger . toInteger . (.&. 0xFF)

instance Class Word16 where
  fromWord64 = fromInteger . toInteger . (.&. 0xFFFF)

instance Class Word32 where
  fromWord64 = fromInteger . toInteger . (.&. 0xFFFFFFFF)
  
instance Class Word64 where
  fromWord64 x = x
