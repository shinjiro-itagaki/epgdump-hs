module SITables.TDT (
  Data,
  Class(..),
  pids, table_ids  
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..),SITableIDs(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import Common(EmptyExist(..),PID,TableID,PIDs(..))
import qualified BytesReader.Base as BytesReaderBase
import qualified Descriptor
import qualified SITables.Base as Base
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import qualified Parser
import FromWord64 hiding (Class)

class (Header1.Class a) => Class a where
  jst_time :: a -> Word64

data Data = MkData {
  -- CommonHeader 
  _header1 :: Header1.Data,
  _jst_time :: Word64
  } deriving (Show)

instance Header1.Class Data where
  setHeader1 x h = x { _header1 = h }
  header1 = _header1
  
instance Class Data where
  jst_time = _jst_time 

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty

instance Parser.Class Data where
  
_parseIOFlow :: (BytesReaderBase.Class bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow fh init = do
  getBitsIO_M fh [
    (40, (\(v,d) -> d { _jst_time = fromWord64 v}))
    ] init
  
instance Base.Class Data where
  footer _ = Nothing
  parseIOFlowAfterHeader1 =
    flowStart
    |>>= _parseIOFlow
 
instance SITableIDs Data where
  pids      _ = MkPIDs [0x0014]
  table_ids _ = [0x70]
