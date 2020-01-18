module Descriptor.Component (
    Class(..)
    ,Data
    ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Descriptor.CountryCode as CountryCode
import qualified Descriptor.LangCode as LangCode
import Data.Vector(Vector,empty,toList,snoc)

class (Base.Class a) => Class a where
  stream_content :: a -> Word8
  component_type :: a -> Word8
  component_tag  :: a -> Word8
  lang_code      :: a -> LangCode.Data
  text           :: a -> String

data Data = MkData {
  _header            :: Header.Data,
  _stream_content    :: Word8,
  _component_type    :: Word8,
  _component_tag     :: Word8,
  _iso_639_language_code :: LangCode.Data,
  _text              :: String
}

instance Base.Class Data where
--  fromByteString bs = (Nothing, bs)

instance Class Data where
  stream_content = _stream_content
  component_type = _component_type
  component_tag  = _component_tag
  lang_code      = _iso_639_language_code
  text           = _text
