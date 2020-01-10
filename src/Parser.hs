module Parser where

import Data.Word(Word64, Word32, Word16, Word8)
import Common(EmptyExist(..),FromWord64(..))

type BitLen = Word8

data (Eq sym, EmptyExist state) => StateUpdater sym state = MkStateUpdater (sym -> Word64 -> state -> state)
data (EmptyExist state) => StateToResult state result = MkStateToResult (state -> Maybe result)

data (EmptyExist state) => CacheInfo sym state result = MkCacheInfo state (StateUpdater sym state) (StateToResult state result) -- 状態の具体的なデータ、状態を更新する関数 状態を最終結果に変換するための関数

cacheToResult :: (EmptyExist state) => (CacheInfo sym state result) -> Maybe result
cacheToResult (MkCacheInfo st _ (MkStateToResult f)) = f st

data (Eq symbol, EmptyExist state) => ParseCondition symbol state result =
  ParseStart (ParseCondition symbol state result) -- 一番最初のパース条件を返す関数を含んだコンストラクタ
  | ParseFinished (Maybe result) -- パースを終了して状態から成果物を作成するためのコンストラクタ
  | NextParse
    BitLen -- 取得するビットの長さ  
    symbol -- 対象になっている項目を示す何か
    (CacheInfo symbol state result) -- キャッシュの内容一式
    (Word64 -> symbol -> (CacheInfo symbol state result) -> ParseCondition symbol state result) -- パースした結果を受け取り、対象になっている項目、状態、を入力すると次のパース条件を返す関数

class (Eq a) => ParseConditionSymbol a where
  conditionDefines :: [(a,Word8)]
  firstCondition :: (EmptyExist state) => StateUpdater a state -> StateToResult state result -> ParseCondition a cache result
  
  findNext :: a -> Maybe a
  findNext sym = impl conditionDefines
   where
     impl []     = Nothing
     impl (x:[]) = Nothing
     impl (x:(y:ys)) = if (fst x) == sym then Just $ fst y else impl (y:ys)

  _generateNextCondition :: (EmptyExist state) => (Maybe a -> CacheInfo a state result -> ParseCondition a state result) -> Word64 -> a -> CacheInfo a state result -> ParseCondition a state result
  _generateNextCondition cond_finder rawv sym (MkCacheInfo state updater@(MkStateUpdater updaterf) fx) = cond_finder nextsym (MkCacheInfo newstate updater fx)
    where
      nextsym = findNext sym
      newstate = updaterf sym rawv state

  generateNextCondition :: (EmptyExist state) => (Maybe a -> CacheInfo a state result -> ParseCondition a state result) -> Word64 -> a -> state -> ParseCondition a state result

  nextConditionGenerator :: (EmptyExist state) => Word64 -> a -> CacheInfo a state result -> ParseCondition a state result
  nextConditionGenerator = _generateNextCondition findParseCondition

  findParseCondition :: (EmptyExist state) => Maybe a -> CacheInfo a state result -> ParseCondition a state result
  findParseCondition Nothing    cache = ParseFinished $ cacheToResult cache
  findParseCondition (Just sym) cache = findParseConditionImpl sym cache conditionDefines

  findParseConditionImpl :: (EmptyExist state) => a -> CacheInfo a state result -> [(a,Word8)] -> ParseCondition a state result
  findParseConditionImpl   _ cache     [] = ParseFinished $ cacheToResult cache
  findParseConditionImpl sym cache (x:[]) = ParseFinished $ cacheToResult cache
  findParseConditionImpl sym cache ((xsym,xlen):xs)
    | sym == xsym = NextParse xlen xsym cache nextConditionGenerator
    | otherwise = findParseConditionImpl sym cache xs

-- data CommonHeaderCache = MkCommonHeaderCache {
--   _m_table_id                 :: Maybe (Word8,CommonHeaderSymbol),
--   _m_section_syntax_indicator :: Maybe Bool,
--   _m_reserved_future_use      :: Maybe Bool,
--   _m_reserved1                :: Maybe Word8,
--   _m_section_length           :: Maybe Word16
--   }
