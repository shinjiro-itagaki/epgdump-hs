module SITables.BIT (
  Data,
  Class(..),
  pids,
  table_ids
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..),SITableIDs(..))
import qualified Schedule
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import Common(EmptyExist(..),PID,TableID,BytesHolderIO(..),TableID,PID,PIDs(..),BytesLen)
import qualified Descriptor
import qualified SITables.Base as Base
import Parser(FromWord64(..),ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import qualified Parser
import Data.Vector(Vector,toList,empty,snoc)
import qualified SITables.BIT.Item as Item

class (Base.Class a, Header2.Class a, Footer.Class a) => Class a where
  original_network_id      :: a -> Word16
  reserved_future_use      :: a -> Word8 
  broadcast_view_propriety :: a -> Bool
  first_descriptors_length :: a -> BytesLen
  descriptors              :: a -> [Descriptor.Data]
  items                    :: a -> [Item.Data]

data Data = MkData {
  _header1                  :: Header1.Data,
  _original_network_id      :: Word16,
  _header2                  :: Header2.Data,
  _reserved_future_use      :: Word8,
  _broadcast_view_propriety :: Bool,
  _first_descriptors_length :: BytesLen,
  _descriptors              :: Vector Descriptor.Data,
  _items                    :: Vector Item.Data,
  _footer                   :: Footer.Data
  }

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
  original_network_id      = _original_network_id  
  reserved_future_use      = _reserved_future_use
  broadcast_view_propriety = _broadcast_view_propriety 
  first_descriptors_length = _first_descriptors_length 
  descriptors              = toList . _descriptors
  items                    = toList . _items

instance SITableIDs Data where
  pids      _ = MkPIDs [0x0024]
  table_ids _ = [0xC4]

instance EmptyExist Data where
  mkEmpty =  MkData {
    _header1                  = mkEmpty,
    _original_network_id      = mkEmpty,
    _header2                  = mkEmpty,
    _reserved_future_use      = mkEmpty,
    _broadcast_view_propriety = mkEmpty,
    _first_descriptors_length = mkEmpty,
    _descriptors              = Data.Vector.empty,
    _items                    = Data.Vector.empty,
    _footer                   = mkEmpty
  }

instance Parser.Class Data where

_parseIOFlow2 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow2 fh init =
  getBitsIO_M fh [
  (16, (\(v,d) -> d { _original_network_id = fromWord64 v}))
  ] init

_parseIOFlow4 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow4 fh init = do
  getBitsIO_M fh [
    ( 8, (\(v,d) -> d { _reserved_future_use      = fromWord64 v})),
    ( 3, (\(v,d) -> d { _broadcast_view_propriety = fromWord64 v})),
    ( 1, (\(v,d) -> d { _first_descriptors_length = fromWord64 v}))
    ] init

_parseIOFlow5_descs :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow5_descs bh init = Descriptor.gather addItem' (first_descriptors_length init) bh init
  where
    addItem' :: Data -> Descriptor.Data -> Data
    addItem' x desc = x {_descriptors = (snoc (_descriptors x) desc)  }
  
instance Base.Class Data where
  footer  = Just . _footer
  parseIOFlowAfterHeader1 =
    flowStart
    |>>= _parseIOFlow2
    |>>= Header2.parseFlow
    |>>= _parseIOFlow4
    |>>= _parseIOFlow5_descs
    |>>= Footer.parseFlow
