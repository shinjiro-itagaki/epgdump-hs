-- 6.2.3
module Descriptor.Component (
    Class(..)
    ,Data
    ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Utils.LangCode as LangCode
import Data.Vector(Vector,empty,toList,snoc)
import Utils.ToWord64(toWord64)
import Data.Bits(Bits,shiftR,shiftL,(.&.))
import Data.ByteString.Lazy as BS
import Utils.FromByteString(fromByteString)
import Utils.ToString(toString)
import qualified Parser.Result as Result

import qualified Utils.ComponentType as ComponentType

class (Base.Class a) => Class a where
  stream_content :: a -> Word8
  component_type :: a -> ComponentType.Data
  component_tag  :: a -> Word8
  lang_code      :: a -> LangCode.Data
  text           :: a -> String

data Data = MkData {
  _header            :: Header.Data,
  _stream_content    :: ComponentType.StreamContent,
  _component_type    :: ComponentType.RawData,
  _component_tag     :: Word8,
  _iso_639_language_code :: LangCode.Data,
  _text              :: Common.ByteString
} deriving (Show)

instance Header.Class Data where
  header = _header

_filter :: (Bits a, Integral a, Num b) => a -> a -> Int -> b
_filter x filter offset = fromInteger $ toInteger $ (`shiftR` offset) $ x .&. (filter `shiftL` (offset * 4))

instance Base.Class Data where
  fromByteStringAfterHeader h bs =
    let (bs1,rest) = BS.splitAt 3 bs
        v = toWord64 bs1
        len = (Header.descriptor_length h) - 3
        stream_content'        = _filter v 0x00000F 9
        component_type'        = _filter v 0x00000F 8
        component_tag'         = _filter v 0x0000FF 6
        
        (bs2,rest2) = BS.splitAt 3 bs
        iso_639_language_code' = fromByteString bs2
        d =  MkData { _header = h,
                      _stream_content = stream_content',
                      _component_type = component_type',
                      _component_tag  = component_tag',
                      _iso_639_language_code = iso_639_language_code',
                      _text = rest2
                    }
    in Result.Parsed d 

instance Class Data where
  stream_content = _stream_content
  component_type x =
    let x1 = _stream_content x
        x2 = _component_type x
    in ComponentType.toData x1 x2
  component_tag  = _component_tag
  lang_code      = _iso_639_language_code
  text           = toString . _text
