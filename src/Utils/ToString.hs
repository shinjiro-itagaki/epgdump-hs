{-# LANGUAGE FlexibleInstances #-}
module Utils.ToString where
import Common(ByteString)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import qualified Data.ByteString.Lazy as BS
import Data.Word(Word64, Word32, Word16, Word8)
import qualified Data.ByteString.Lazy.Char8 as BChar8

class Class a where
  toString :: a -> String

instance Class [Word8] where
  toString = BChar8.unpack . BS.pack

instance Class Word8 where
  toString = toString . (:[])

instance Class ByteString where
  toString = toString . BS.unpack

