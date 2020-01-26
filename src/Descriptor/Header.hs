module Descriptor.Header (
  Class(..)
  ,Data
  ,mk
  ) where
import qualified Utils.EmptyExist as EmptyExist
import qualified Parser.Result as Result
import qualified Data.ByteString as BS
import qualified Utils.FromByteString as FromByteString
import Utils

class (Show a) => Class a where
  header            :: a -> Data
  descriptor_tag    :: a -> Word8
  descriptor_tag = descriptor_tag . header
  descriptor_length :: (Num b) => a -> b
  descriptor_length = fromInteger . toInteger . descriptor_length . header

data Data = MkData {
  _descriptor_tag    :: Word8,
  _descriptor_length :: Word8
  } deriving (Show)

mk :: Word8 -> Word8 -> Data
mk x y = MkData {
  _descriptor_tag    = x,
  _descriptor_length = y
  }

instance Class Data where
  header x = x

instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    let (x,bs0) = fromByteStringWithRest bs
        (y,bs1) = fromByteStringWithRest bs0
    in (mk x y, bs1)

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty
