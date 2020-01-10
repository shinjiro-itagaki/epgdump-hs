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
import Common(ParseCondition(..),EmptyExist(..),FromWord64(..))

class CommonHeader a where
  table_id                    :: a -> Word8 -- h->table_id = getBit(data, &boff, 8);
  section_syntax_indicator    :: a -> Bool -- h->section_syntax_indicator = getBit(data, &boff, 1);
  reserved_future_use         :: a -> Bool -- h->reserved_future_use = getBit(data, &boff, 1);
  reserved1                   :: a -> Word8 -- h->reserved1 = getBit(data, &boff, 2);
  section_length              :: a -> Word16 -- h->section_length =getBit(data, &boff,12);
  
-- ParseCondition symbol state vtype result

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
  
data CommonHeaderSymbol = TableID | SectionSyntaxIndicator | ReservedFutureUse | Reserved1 | SectionLength deriving (Eq)
  
type {-(FromWord64 vtype) =>-} CommonHeaderParserCond {-vtype-} = ParseCondition CommonHeaderSymbol CommonHeaderCache {-vtype-} CommonHeaderCache

parserCommonHeaderInfo :: [(CommonHeaderSymbol,Word8)]
parserCommonHeaderInfo = [
  (TableID                , 8),
  (SectionSyntaxIndicator , 1),
  (ReservedFutureUse      , 1),
  (Reserved1              , 2),
  (SectionLength          ,12)
  ]

-- data (FromWord64 vtype) => ParseCondition symbol state vtype result =
--   ParseStart (ParseCondition symbol state vtype result) -- 一番最初のパース条件を返す関数を含んだコンストラクタ
--   | ParseFinished result -- パースを終了して状態から成果物を作成するためのコンストラクタ
--   | NextParse
--     BitLen -- 取得するビットの長さ  
--     symbol -- 対象になっている項目を示す何か
--     state  -- 任意の状態
--     (Word64 -> symbol -> state -> ParseCondition symbol state vtype result) -- パースした結果、対象になっている項目、状態、を入力すると次のパース条件を返す関数

firstParseConditionForCommonHeader :: CommonHeaderParserCond
firstParseConditionForCommonHeader = ParseStart (findCommonHeaderParseCond (Just $ fst $ head $ parserCommonHeaderInfo) mkEmpty)

findNextCommonHeaderSymbol :: CommonHeaderSymbol -> Maybe CommonHeaderSymbol
findNextCommonHeaderSymbol sym = impl parserCommonHeaderInfo
  where
    impl []     = Nothing
    impl (x:[]) = Nothing
    impl (x:(y:ys)) = if (fst x) == sym then Just $ fst y else impl (y:ys)

_generateNextCond :: (Maybe CommonHeaderSymbol -> CommonHeaderCache -> CommonHeaderParserCond) -> Word64 -> CommonHeaderSymbol -> CommonHeaderCache -> CommonHeaderParserCond
_generateNextCond cond_finder rawv sym cache = cond_finder nextsym newcache
  where
    nextsym = findNextCommonHeaderSymbol sym
    newcache = case sym of
      TableID                -> cache{_m_table_id                 = (Just . fromWord64) rawv}
      SectionSyntaxIndicator -> cache{_m_section_syntax_indicator = (Just . fromWord64) rawv}
      ReservedFutureUse      -> cache{_m_reserved_future_use      = (Just . fromWord64) rawv}
      Reserved1              -> cache{_m_reserved1                = (Just . fromWord64) rawv}
      SectionLength          -> cache{_m_section_length           = (Just . fromWord64) rawv}

findCommonHeaderParseCond :: Maybe CommonHeaderSymbol -> CommonHeaderCache -> CommonHeaderParserCond
findCommonHeaderParseCond Nothing    cache = ParseFinished cache
findCommonHeaderParseCond (Just sym) cache = impl sym cache parserCommonHeaderInfo
  where
    next_cond_generator = _generateNextCond findCommonHeaderParseCond
    impl :: CommonHeaderSymbol -> CommonHeaderCache -> [(CommonHeaderSymbol,Word8)] -> CommonHeaderParserCond
    impl   _ cache     [] = ParseFinished cache
    impl sym cache (x:[]) = ParseFinished cache
    impl sym cache ((xsym,xlen):xs)
      | sym == xsym = NextParse xlen xsym cache next_cond_generator
      | otherwise = impl sym cache xs

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
