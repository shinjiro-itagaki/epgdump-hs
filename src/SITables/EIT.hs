{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.EIT(
  Data,
  Class(..),
  pids,
  table_ids,
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import qualified SITables.Base as Base
import SITables.Common(HasDescriptors(..))
import qualified SITables.Items as Items
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import Common(HasOriginalNetworkID(..),EmptyExist(..),PID,TableID,BytesHolderIO(..))
import Descriptor(HasServiceID(..))
import Parser(HasParser(..),FromWord64(..),ParseResult(..))
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty,snoc)
import Data.Maybe(fromMaybe)

import qualified SITables.EIT.Item as Item

pids :: [PID]
pids = [0x0012,0x0026,0x0027]

table_ids :: [TableID]
table_ids = [0x4E,0x4F] ++ [0x50..0x5F] ++ [0x60..0x6F]

class (Base.Class a, Header1.Class a, Header2.Class a, HasOriginalNetworkID a, HasServiceID a) => Class a where
  transport_stream_id         :: a -> Word16
  segment_last_section_number :: a -> Word8
  last_table_id               :: a -> Word8

data Data = MkData {
  _header1                     :: Header1.Data,
  _service_id                  :: Word16,
  _header2                     :: Header2.Data,
  _transport_stream_id         :: Word16,
  _original_network_id         :: Word16,
  _segment_last_section_number :: Word8,
  _last_table_id               :: Word8,
  _items                       :: Vector Item.Data, 
  _footer                      :: Footer.Data
  }

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty empty mkEmpty

instance Header1.Class Data where
  header1 = _header1
  setHeader1 x h = x {_header1 = h}  
  
instance Header2.Class Data where
  header2 = _header2
  setHeader2 x h = x {_header2 = h}

instance Footer.Class Data where
  footer = _footer
  setFooter x y = x {_footer = y}

instance HasOriginalNetworkID Data where
  original_network_id = _original_network_id

instance HasServiceID Data where
  service_id = _service_id

instance Base.Class Data where
  header1 = _header1
  footer  = Just . _footer

instance Class Data where
  transport_stream_id         = _transport_stream_id
  segment_last_section_number = _segment_last_section_number
  last_table_id               = _last_table_id

_parseIOFlow2 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow2 fh init =
  getBitsIO_M fh [
  (16, (\(v,d) -> d { _service_id = fromWord64 v}))
  ] init

_parseIOFlow4 fh init = do
  getBitsIO_M fh [
    (16, (\(v,d) -> d { _transport_stream_id         = fromWord64 v})),
    (16, (\(v,d) -> d { _original_network_id         = fromWord64 v})),
    ( 8, (\(v,d) -> d { _segment_last_section_number = fromWord64 v})),
    ( 8, (\(v,d) -> d { _last_table_id               = fromWord64 v}))
    ] init

_parseIOFlow5_items :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow5_items bh init = Items.gather addItem' (Base.section_length_without_crc init) bh init
  where
    addItem' :: Data -> Item.Data -> Data
    addItem' x item = x {_items = (snoc (_items x) item)  }

instance HasParser Data where
  parseIOFlow =
    flowStart
    |>>= Header1.parseFlow
    |>>= _parseIOFlow2
    |>>= Header2.parseFlow
    |>>= _parseIOFlow4
    |>>= _parseIOFlow5_items
    |>>= Footer.parseFlow
