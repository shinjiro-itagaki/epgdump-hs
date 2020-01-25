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
  _text                   :: String  
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  iso_639_language_code  = _iso_639_language_code  
  descriptor_number      = _descriptor_number
  last_descriptor_number = _last_descriptor_number
  length_of_items        = _length_of_items
  items                  = toList . _items
  text_length            = _text_length
  text = _text
