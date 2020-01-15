{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.EIT(
  Data,
  Class(..),
  Item,
  pids,
  table_ids,
  parse,
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import SITables.Common(HasDescriptors(..))
import qualified SITables.Header1 as Header1
import qualified SITables.Header2 as Header2
import qualified SITables.Footer as Footer
import Common(HasOriginalNetworkID(..),EmptyExist(..),PID,TableID)
import Descriptor(HasServiceID(..))
import Parser(or, HasParser(..),ParseResult(..),ParseConditionSymbol(..),ValueCache,FromValueCache(..))
import qualified Descriptor
import Data.ByteString(ByteString)
import Data.Vector(Vector,toList,empty)

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
  _footer                      :: Footer.Data
  }

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty

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

class (HasDescriptors a) => Item a where
  event_id                :: a -> Word16
  start_time              :: a -> Word64
  duration                :: a -> Word32
  running_status          :: a -> Word8
  free_CA_mode            :: a -> Bool
  descriptors_loop_length :: a -> Word16

data ItemData = MkItemData {
  _event_id                :: Word16,
  _start_time              :: Word64,
  _duration                :: Word32,
  _running_status          :: Word8,
  _free_CA_mode            :: Bool,
  _descriptors_loop_length :: Word16,
  _descriptors             :: Vector Descriptor.Data
  }

instance HasDescriptors ItemData where
  descriptors = toList . _descriptors

instance Item ItemData where
  event_id                = _event_id 
  start_time              = _start_time
  duration                = _duration
  running_status          = _running_status
  free_CA_mode            = _free_CA_mode
  descriptors_loop_length = _descriptors_loop_length

instance EmptyExist ItemData where
  mkEmpty = MkItemData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty Data.Vector.empty

data Symbol = Header1 | ServiceID | Header2 | TransportStreamID | OriginalNetworkID | SegmentLastSectionNumber | LastTableID | Items | Footer deriving (Eq,Enum,Bounded)

instance ParseConditionSymbol Symbol where
  getLen Header1                  = Header1.length
  getLen ServiceID                = 16
  getLen Header2                  = Header2.length
  getLen TransportStreamID        = 16
  getLen OriginalNetworkID        = 16
  getLen SegmentLastSectionNumber = 8
  getLen LastTableID              = 8
  getLen Items                    = bitsLength (allSymbols :: [ItemSymbol])
  getLen Footer                   = Footer.length

update :: Symbol -> ValueCache -> Data -> (Data,Maybe Symbol)
-- 取得したByteStringでparseを実行
update Header1                  v old = (old {_header1                     = (`Parser.or` mkEmpty) $ fst $ parse $ fst v} ,Nothing)
update Header2                  v old = (old {_header2                     = (`Parser.or` mkEmpty) $ fst $ parse $ fst v} ,Nothing)
update ServiceID                v old = (old {_service_id                  = fromValueCache v}                      ,Nothing)
update TransportStreamID        v old = (old {_transport_stream_id         = fromValueCache v}                      ,Nothing)
update OriginalNetworkID        v old = (old {_original_network_id         = fromValueCache v}                      ,Nothing)
update SegmentLastSectionNumber v old = (old {_segment_last_section_number = fromValueCache v}                      ,Nothing)
update LastTableID              v old = (old {_last_table_id               = fromValueCache v}                      ,Nothing)
update Footer                   v old = (old {_footer                      = (`Parser.or` mkEmpty) $ fst $ parse $ fst v} ,Nothing)

result :: a -> Maybe a
result x = Just x

instance HasParser Data where
  parse = startParse update result
--  parseIO bh = do

data ItemSymbol = EventID | StartTime | Duration | RunningStatus | FreeCA_Mode | DescriptorLoopLength deriving (Eq,Enum,Bounded)

instance ParseConditionSymbol ItemSymbol where
  getLen EventID              = 16
  getLen StartTime            = 40
  getLen Duration             = 24
  getLen RunningStatus        = 3
  getLen FreeCA_Mode          = 1
  getLen DescriptorLoopLength = 12

update2 :: ItemSymbol -> ValueCache -> ItemData -> (ItemData,Maybe ItemSymbol)
update2 EventID              v old = (old {_event_id       = fromValueCache v}, Nothing)
update2 StartTime            v old = (old {_start_time     = fromValueCache v}, Nothing)
update2 Duration             v old = (old {_duration       = fromValueCache v}, Nothing)
update2 RunningStatus        v old = (old {_running_status = fromValueCache v}, Nothing)
update2 FreeCA_Mode          v old = (old {_free_CA_mode   = fromValueCache v}, Nothing)
update2 DescriptorLoopLength v old = (old                                     , Nothing) -- not implemented
-- update Footer                   v old = (old {_footer                      = (`Parser.or` mkEmpty) $ parse $ fst v} ,Nothing)

instance HasParser ItemData where
  parse = startParse update2 result
