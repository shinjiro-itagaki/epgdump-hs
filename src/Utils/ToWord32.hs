{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Utils.ToWord32 where
import Common
import qualified Data.ByteString.Lazy as BS

class Class a where
  toWord32 :: a -> Word32

--instance (Integral a, Num a) => Class a where
instance Class Word8 where
  toWord32 = fromInteger . toInteger

instance Class Word16 where
  toWord32 = fromInteger . toInteger

instance Class Word32 where
  toWord32 x = x

instance Class Word64 where
  toWord32 x
    | x > 0xFFFFFFFF = 0xFFFFFFFF
    | otherwise = fromInteger $ toInteger x

--instance (Class a, Bits a) => Class [a] where
instance Class [Word8] where
  toWord32 xs = let bitSize = fromMaybe 8 $ bitSizeMaybe $ Prelude.head xs
                in Prelude.foldl (\rtn w8 -> (rtn `shiftL` bitSize) .|. (toWord32 w8) ) (0 :: Word32) xs

instance Class [Word16] where
  toWord32 xs = let bitSize = fromMaybe 8 $ bitSizeMaybe $ Prelude.head xs
                in Prelude.foldl (\rtn w8 -> (rtn `shiftL` bitSize) .|. (toWord32 w8) ) (0 :: Word32) xs


instance Class ByteString where
  toWord32 = toWord32 . BS.unpack
