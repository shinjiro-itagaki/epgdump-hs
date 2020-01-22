module SITables.SDT(
  Data,
  Class(..),
  pids, table_ids
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..),SITableIDs(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import Common(EmptyExist(..),PID,TableID,TableID,PID,PIDs(..))
import qualified BytesReader.HolderIO as HolderIO
import qualified Descriptor
import qualified SITables.Base as Base
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import FromWord64 hiding (Class)
import qualified Parser
import qualified SITables.Footer as Footer
import qualified SITables.SDT.Item as Item
import Data.Vector(Vector,toList,empty,snoc)
import qualified SITables.Items as Items

class (Header1.Class a, Header2.Class a, Footer.Class a) => Class a where
  original_network_id :: a -> Word16
  transport_stream_id :: a -> Word16
  reserved_future_use :: a -> Word8
  
data Data = MkData {
  _header1             :: Header1.Data,
  _transport_stream_id :: Word16,
  _header2             :: Header2.Data,
  _original_network_id :: Word16,
  _reserved_future_use :: Word8,
  _items               :: Vector Item.Data,
  _footer              :: Footer.Data
  } deriving (Show)

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty empty mkEmpty

instance SITableIDs Data where
  pids      _ = MkPIDs [0x0011]
  table_ids _ = [0x42,0x46]

instance Header1.Class Data where
  header1 = _header1
  setHeader1 x h = x {_header1 = h}    
  
instance Header2.Class Data where
  header2 = _header2
  setHeader2 x h = x {_header2 = h}

instance Footer.Class Data where
  footer = _footer
  setFooter x y = x {_footer = y}

instance Class Data where
  original_network_id = _original_network_id
  transport_stream_id = _transport_stream_id
  reserved_future_use = _reserved_future_use

instance Parser.Class Data where

_parseIOFlow2 fh init =
  getBitsIO_M fh [
  (16, (\(v,d) -> d { _transport_stream_id = fromWord64 v}))
  ] init

_parseIOFlow3 fh init =
  getBitsIO_M fh [
  (16, (\(v,d) -> d { _original_network_id = fromWord64 v})),
  ( 8, (\(v,d) -> d { _reserved_future_use = fromWord64 v}))
  ] init  

_parseIOFlow4_items :: (HolderIO.Class bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow4_items bh init = Items.gather addItem' (Base.section_length_without_crc init) bh init
  where
    addItem' :: Data -> Item.Data -> Data
    addItem' x item = x {_items = (snoc (_items x) item)  }
  
instance Base.Class Data where
  footer = Just . _footer
  parseIOFlowAfterHeader1 =
    flowStart
    |>>= _parseIOFlow2
    |>>= Header2.parseFlow
    |>>= _parseIOFlow3
    |>>= _parseIOFlow4_items
    |>>= Footer.parseFlow
