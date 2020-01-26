module Descriptor.ExtendedEvent.Item (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Utils.FromByteString as FromByteString
import qualified Data.ByteString.Lazy as BS
import Utils.ToString(toString)

class (FromByteString.Class a) => Class a where
  item_description_length :: a -> Word8
  item_description_chars  :: a -> String
  item_length             :: a -> Word8
  item_chars              :: a -> String  
  
data Data = MkData {
  _item_description_length :: Word8,
  _item_description_chars  :: ByteString,
  _item_length             :: Word8,
  _item_chars              :: ByteString
  } deriving (Show)

instance Class Data where
  item_description_length = _item_description_length
  item_description_chars  = toString . _item_description_chars
  item_length             = _item_length
  item_chars              = toString . _item_chars
    
instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    let (item_description_length,bs0) = FromByteString.fromByteStringWithRest bs
        (item_description_chars ,bs1) = BS.splitAt (fromInteger $ toInteger item_description_length) bs0
        (item_length,bs2)             = FromByteString.fromByteStringWithRest bs1
        (item_chars,rest)             = BS.splitAt (fromInteger $ toInteger item_length) bs2
        d = MkData {
          _item_description_length = item_description_length,
          _item_description_chars  = item_description_chars,
          _item_length             = item_length,
          _item_chars              = item_chars
          }
    in (d, rest)

