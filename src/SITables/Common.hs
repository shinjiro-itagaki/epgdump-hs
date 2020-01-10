{-# LANGUAGE FlexibleInstances #-}

module SITables.Common(
  CommonHeader(..)
  ,CommonHeader2(..)
  ,HasDescriptors(..)
  ,Schedule(..)
  ,MatchPID(..)
  ,MatchTableID(..)
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Descriptor
import Common(EmptyExist(..))
import Parser(ParseConditionSymbol(..),FromWord64(..))

class CommonHeader a where
  table_id                    :: a -> Word8 -- h->table_id = getBit(data, &boff, 8);
  section_syntax_indicator    :: a -> Bool -- h->section_syntax_indicator = getBit(data, &boff, 1);
  reserved_future_use         :: a -> Bool -- h->reserved_future_use = getBit(data, &boff, 1);
  reserved1                   :: a -> Word8 -- h->reserved1 = getBit(data, &boff, 2);
  section_length              :: a -> Word16 -- h->section_length =getBit(data, &boff,12);

data CommonHeaderCache = MkCommonHeaderCache {
  _m_table_id                 :: Maybe Word8,
  _m_section_syntax_indicator :: Maybe Bool,
  _m_reserved_future_use      :: Maybe Bool,
  _m_reserved1                :: Maybe Word8,
  _m_section_length           :: Maybe Word16
  }

instance EmptyExist CommonHeaderCache where
  mkEmpty = MkCommonHeaderCache {
    _m_table_id                 = Nothing,
    _m_section_syntax_indicator = Nothing,
    _m_reserved_future_use      = Nothing,
    _m_reserved1                = Nothing,
    _m_section_length           = Nothing
    }
  
data CommonHeaderSymbol = TableID | SectionSyntaxIndicator | ReservedFutureUse | Reserved1 | SectionLength deriving (Eq,Enum,Bounded)

_stateUpdater :: CommonHeaderSymbol -> Word64 -> CommonHeaderCache -> CommonHeaderCache
_stateUpdater sym v st =
  case sym of
    TableID                -> st { _m_table_id                 = Just $ fromWord64 v }
    SectionSyntaxIndicator -> st { _m_section_syntax_indicator = Just $ fromWord64 v }
    ReservedFutureUse      -> st { _m_reserved_future_use      = Just $ fromWord64 v }
    Reserved1              -> st { _m_reserved1                = Just $ fromWord64 v }
    SectionLength          -> st { _m_section_length           = Just $ fromWord64 v }

instance ParseConditionSymbol CommonHeaderSymbol where
  getLen TableID                = 8
  getLen SectionSyntaxIndicator = 1
  getLen ReservedFutureUse      = 1
  getLen Reserved1              = 2
  getLen SectionLength          =12

class CommonHeader2 a where
  reserved2                   :: a -> Word8 -- h->reserved2 = getBit(data, &boff, 2);
  version_number              :: a -> Word8 -- h->version_number = getBit(data, &boff, 5);
  current_next_indicator      :: a -> Bool -- h->current_next_indicator = getBit(data, &boff, 1);
  section_number              :: a -> Word8 -- h->section_number = getBit(data, &boff, 8);
  last_section_number         :: a -> Word8 -- h->last_section_number = getBit(data, &boff, 8);

-- firstParseConditionForCommonHeader2 :: ParseCondition

class HasDescriptors a where
  descriptors :: a -> [Descriptor.Data]

--data TransportStream = MkTransportStream {
--  transport_stream_id :: Word16,
--  original_network_id :: Word16,
--  transport_descriptors_length :: Word16
--  }

class Schedule a where
  start_time :: a -> Word64
  duration   :: a -> Word32

class MatchPID a where
  match_pid :: a -> Word64 -> Bool

class MatchTableID a where
  match_table_id :: a -> Word32 -> Bool

instance MatchPID [Word64] where
  match_pid (x:[]) y = False
  match_pid (x:xs) y = if x == y then True else match_pid xs y

instance MatchTableID [Word32] where
  match_table_id (x:[]) y = False
  match_table_id (x:xs) y = if x == y then True else match_table_id xs y
