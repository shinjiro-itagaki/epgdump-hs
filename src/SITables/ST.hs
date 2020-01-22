module SITables.ST(
  Data,
  Class(..),
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(SITableIDs(..))
import qualified SITables.Base as Base
import Common(ByteString,PIDs(..),TableID,EmptyExist(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified Descriptor
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import FromWord64 hiding (Class)
import qualified Parser
import qualified SITables.Base as Base

class (Base.Class a) => Class a where
  data_bytes :: a -> ByteString

data Data = MkData {
  _header1    :: Header1.Data,
  _data_bytes :: ByteString
  } deriving (Show)

instance SITableIDs Data where
  pids      _ =  MkExcludePIDs [0x0000,0x0001,0x0014]
  table_ids _ = [0x72]

instance Base.Class Data where
  footer  _ = Nothing

instance Class Data where
  data_bytes = _data_bytes

instance Header1.Class Data where
  setHeader1 x header = x {_header1 = header}
  header1 = _header1

instance Parser.Class Data where

instance EmptyExist Data where
  mkEmpty = MkData {
  _header1    = mkEmpty,
  _data_bytes = mkEmpty
  }
  
