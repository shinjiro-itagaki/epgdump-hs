{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Common where
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString(ByteString,pack,unpack,null,uncons,empty)
import Data.Bits((.&.),shiftL,shiftR)
import Data.Char(chr)

class HasOriginalNetworkID a where
  original_network_id :: a -> Word16
  
class EmptyExist a where
  mkEmpty :: a
instance EmptyExist ByteString where
  mkEmpty = Data.ByteString.empty
instance EmptyExist Word64 where
  mkEmpty = 0
instance EmptyExist Word32 where
  mkEmpty = 0  
instance EmptyExist Word16 where
  mkEmpty = 0  
instance EmptyExist Word8  where
  mkEmpty = 0

--type Hoge a = (a,a)
--newtype HexRGBA = HexRGBA (Hoge Word32)
--color :: HexRGBA
--color = HexRGBA (0xFFFFFFFF,0xFFFFFFFF)
