-- 6.2.7
module Descriptor.ExtendedEvent (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)
import qualified Descriptor.ExtendedEvent.Item as Item
import qualified Utils.LangCode as LangCode
import Utils
import qualified Data.ByteString.Lazy as BS
import qualified Parser.Result as Result

class (Base.Class a) => Class a where
  descriptor_number       :: a -> Word8
  last_descriptor_number  :: a -> Word8
  iso_639_language_code   :: a -> LangCode.Data
  length_of_items         :: a -> Word8
  items                   :: a -> [Item.Data]
  text_length             :: a -> Word8
  text                    :: a -> String

data Data = MkData {
  _header                 :: Header.Data, 
  _descriptor_number      :: Word8,
  _last_descriptor_number :: Word8,
  _iso_639_language_code  :: LangCode.Data,
  _length_of_items        :: Word8,
  _items                  :: Vector Item.Data,
  _text_length            :: Word8,
  _text                   :: ByteString
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
  fromByteStringAfterHeader h bs0 =
    let ((w8
         ,iso_639_language_code
         ,length_of_items),bs6) = fromByteStringWithRest bs0
        descriptor_number       = (w8 .&. 0xF0) `shiftR` 4
        last_descriptor_number  = (w8 .&. 0x0F)
        (bs7,bs8)               = BS.splitAt (fromInteger $ toInteger length_of_items) bs6
        items                   = fromByteString bs7
        (text_length,rest)      = fromByteStringWithRest bs8
        text                    = BS.take (fromInteger $ toInteger text_length) rest
        d = MkData {
          _header                 = h,
          _descriptor_number      = descriptor_number,
          _last_descriptor_number = last_descriptor_number,
          _iso_639_language_code  = iso_639_language_code,
          _length_of_items        = length_of_items,
          _items                  = items,
          _text_length            = text_length,
          _text                   = text
          }
    in Result.Parsed d

instance Class Data where
  iso_639_language_code  = _iso_639_language_code  
  descriptor_number      = _descriptor_number
  last_descriptor_number = _last_descriptor_number
  length_of_items        = _length_of_items
  items                  = toList . _items
  text_length            = _text_length
  text                   = toString . _text
