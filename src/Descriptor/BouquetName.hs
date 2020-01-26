-- 6.2.1
module Descriptor.BouquetName (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Utils.LangCode as LangCode
import Data.Vector(Vector,empty,toList,snoc)
import qualified Data.ByteString.Lazy.Char8 as BChar8
import qualified Data.ByteString.Lazy as BS
import Utils.ToString(toString)
import qualified Parser.Result as Result

class (Base.Class a) => Class a where
  name :: a -> String
  
data Data = MkData {
  _header :: Header.Data,
  _name   :: ByteString
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
  fromByteStringAfterHeader h bs =
    Result.Parsed $ MkData h bs

instance Class Data where
  name = toString . _name
