{-# LANGUAGE FlexibleInstances #-}
module Utils.FromByteString where
import Common(ByteString)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import qualified Data.ByteString.Lazy as BS
import Data.Word(Word64, Word32, Word16, Word8)
import qualified Data.ByteString.Lazy.Char8 as BChar8
import qualified Utils.LangCode as LangCode
import Data.Int(Int64)
import Utils.ToWord64(toWord64)
import Utils.ToWord32(toWord32)
import Utils.ToWord16(toWord16)
import Utils.ToWord8(toWord8)

class Class a where
  fromByteStringWithRest :: ByteString -> (a,ByteString)
  fromByteString :: ByteString -> a
  fromByteString = fst . fromByteStringWithRest

instance Class LangCode.Data where
  fromByteStringWithRest x = (\(y,z) -> (impl' $ BChar8.unpack y,z)) $ BS.splitAt 3 x
    where
      impl' []                = ('\0','\0','\0')
      impl' (x1:[])           = ('\0','\0',x1  )
      impl' (x1:(x2:[]))      = ('\0',x1  ,x2  )
      impl' (x1:(x2:(x3:xs))) = (x1  ,x2  ,x3  )

instance Class Word64 where
  fromByteStringWithRest x = (\(y,z) -> (toWord64 $ BS.unpack y,z)) $ BS.splitAt 8 x

instance Class Word32 where
  fromByteStringWithRest x = (\(y,z) -> (toWord32 $ BS.unpack y,z)) $ BS.splitAt 4 x

instance Class Word16 where
  fromByteStringWithRest x = (\(y,z) -> (toWord16 $ BS.unpack y,z)) $ BS.splitAt 2 x

instance Class Word8 where
  fromByteStringWithRest x = (\(y,z) -> (toWord8  $ BS.unpack y,z)) $ BS.splitAt 1 x
