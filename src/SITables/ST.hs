module SITables.ST(
  Data,
  Class(..),
  pids, table_ids
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(SITableIDs(..))
import qualified SITables.Base as Base
import Common(ByteString,HasOriginalNetworkID(..),PIDs(..),TableID,EmptyExist(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import Descriptor(HasServiceID(..),HasEventID(..))
import qualified Descriptor
import Parser(HasParser(..),FromWord64(..),ParseResult(..),ParseIOFlow(..),(>>==))

class (Base.Class a) => Class a where
  data_bytes :: a -> ByteString

data Data = MkData {
  _header1    :: Header1.Data,
  _data_bytes :: ByteString
  }

instance SITableIDs Data where
  pids      _ =  MkExcludePIDs [0x0000,0x0001,0x0014]
  table_ids _ = [0x72]

instance Base.Class Data where
  header1 = _header1
  footer  _ = Nothing

instance Class Data where
  data_bytes = _data_bytes

instance Header1.Class Data where
  setHeader1 x header = x {_header1 = header}
  header1 = _header1

instance HasParser Data where

instance EmptyExist Data where
  mkEmpty = MkData {
  _header1    = mkEmpty,
  _data_bytes = mkEmpty
  }
  
