-- 6.2.2
module Descriptor.CAIdentifier (
  Class(..)
  ,Data
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import qualified Utils.LangCode as LangCode
import qualified Data.Vector as V
import qualified Parser.Result as Result

class Base.Class a => Class a where
  ca_system_id :: a -> [Word16]
  
data Data = MkData {
  _header       :: Header.Data,
  _ca_system_id :: V.Vector Word16
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where
  fromByteStringAfterHeader h bs =
    let (m_v2,rest) = Base.gatherWord16 (Header.descriptor_length h) V.snoc V.empty bs
    in Result.Parsed $ MkData h $ case m_v2 of
      Nothing -> V.empty
      Just v2 -> v2

instance Class Data where  
  ca_system_id = V.toList . _ca_system_id

