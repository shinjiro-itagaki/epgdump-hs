module SITables.NBIT (
  Data,
  Class(..)
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import qualified SITables.Items as Items
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import Common(EmptyExist(..),PID,TableID,BytesHolderIO(..),TableID,PID,PIDs(..))
import qualified Descriptor
import qualified SITables.Base as Base
import SITables.Common(HasDescriptors(..),SITableIDs(..))
import Data.Vector(Vector,toList,empty,snoc)
import qualified SITables.NBIT.Item as Item
import Parser(FromWord64(..),ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import qualified Parser

class (Header1.Class a, Header2.Class a) => Class a where
  original_network_id :: a -> Word16
  
data Data = MkData {
  _header1             :: Header1.Data,
  _original_network_id :: Word16,  
  _header2             :: Header2.Data,
  _items               :: Vector Item.Data,
  _footer              :: Footer.Data
  }

instance SITableIDs Data where
  pids      _ = MkPIDs [0x0025]
  table_ids _ = [0xC5,0xC6]

instance Header1.Class Data where
  setHeader1 d h = d { _header1 = h }
  header1 = _header1
  
instance Header2.Class Data where
  setHeader2 d h = d { _header2 = h }  
  header2 = _header2

instance Footer.Class Data where
  setFooter d x = d { _footer = x }  
  footer = _footer

instance Class Data where
  original_network_id = _original_network_id  

instance EmptyExist Data where
  mkEmpty = MkData {
  _header1             = mkEmpty,
  _original_network_id = mkEmpty,
  _header2             = mkEmpty,
  _items               = Data.Vector.empty,
  _footer              = mkEmpty
  }
  
instance Parser.Class Data where

_parseIOFlow2 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow2 fh init =
  getBitsIO_M fh [
  (16, (\(v,d) -> d { _original_network_id = fromWord64 v}))
  ] init

_parseIOFlow4_items :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
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
    |>>= _parseIOFlow4_items
    |>>= Footer.parseFlow
