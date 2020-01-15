{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.EIT(
  Data,
  Class(..),
  pids,
  table_ids,
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import Common(HasOriginalNetworkID(..),EmptyExist(..),PID,TableID,BytesHolderIO(..))
import Descriptor(HasServiceID(..))
import Parser(HasParser(..),FromWord64(..),ParseResult(..))
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty)
import Data.Maybe(fromMaybe)

import qualified SITables.EIT.Item as Item

pids :: [PID]
pids = [0x0012,0x0026,0x0027]

table_ids :: [TableID]
table_ids = [0x4E,0x4F] ++ [0x50..0x5F] ++ [0x60..0x6F]

class (Header1.Class a, Header2.Class a, HasOriginalNetworkID a, HasServiceID a) => Class a where
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
--  _items                       :: Vector ItemData,
  _item                        :: Item.Data, 
  _footer                      :: Footer.Data
  }

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty

instance Header1.Class Data where
  header1 = _header1
  
instance Header2.Class Data where
  header2 = _header2

instance HasOriginalNetworkID Data where
  original_network_id = _original_network_id

instance HasServiceID Data where
  service_id = _service_id

instance Class Data where
  transport_stream_id         = _transport_stream_id
  segment_last_section_number = _segment_last_section_number
  last_table_id               = _last_table_id

_parseIOFlow1_header1 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow1_header1 fh init = do
  (res_header,fh') <- parseIO fh
  return $ (\x -> (x,fh')) $ mapParseResult res_header $ case res_header of
    Parsed header1 -> init {_header1 = header1}
    _              -> mkEmpty

_parseIOFlow2 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow2 fh init =
  getBitsIO_M fh [
  (16, (\(v,d) -> d { _service_id = fromWord64 v}))
  ] init

_parseIOFlow3_header2 :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow3_header2 fh init = do
  (res_header,fh') <- parseIO fh
  return $ (\x -> (x,fh')) $ mapParseResult res_header $ case res_header of
    Parsed header2 -> init {_header2 = header2}
    _              -> mkEmpty

_parseIOFlow4 fh init = do
  getBitsIO_M fh [
    (16, (\(v,d) -> d { _transport_stream_id         = fromWord64 v})),
    (16, (\(v,d) -> d { _original_network_id         = fromWord64 v})),
    ( 8, (\(v,d) -> d { _segment_last_section_number = fromWord64 v})),
    ( 8, (\(v,d) -> d { _last_table_id               = fromWord64 v}))
    ] init

_parseIOFlow5_item :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow5_item fh init = do
  (res_item,fh') <- parseIO fh
  return $ (\x -> (x,fh')) $ mapParseResult res_item $ case res_item of
    Parsed item -> init {_item = item}
    _           -> mkEmpty

_parseIOFlow6_footer :: (BytesHolderIO bh) => bh -> Data -> IO (ParseResult Data, bh)
_parseIOFlow6_footer fh init = do
  (res_footer,fh') <- parseIO fh
  return $ (\x -> (x,fh')) $ mapParseResult res_footer $ case res_footer of
    Parsed footer -> init {_footer = footer }
    _              -> mkEmpty  

instance HasParser Data where
  parseIOFlow =
    flowStart
    |>>= _parseIOFlow1_header1
    |>>= _parseIOFlow2
    |>>= _parseIOFlow3_header2
    |>>= _parseIOFlow4
    |>>= _parseIOFlow5_item
    |>>= _parseIOFlow6_footer
