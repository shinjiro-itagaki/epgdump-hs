{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser where

import Common(EmptyExist(..))
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString(ByteString,pack,unpack,null,uncons)
import Data.Bits((.&.),shiftL,shiftR)
import Data.Char(chr)

type BitLen = Word8
type ByteHeadValue = Word8
type ByteHeadRestBitCount = Word8
type ByteHead = (ByteHeadValue, ByteHeadRestBitCount)
type ValueCache = Word64

data (Eq sym, EmptyExist state) => StateUpdater sym state = MkStateUpdater (sym -> Word64 -> state -> state)
data (EmptyExist state) => StateToResult state result = MkStateToResult (state -> Maybe result)

data (EmptyExist state) => CacheInfo sym state result = MkCacheInfo state (StateUpdater sym state) (StateToResult state result) -- 状態の具体的なデータ、状態を更新する関数 状態を最終結果に変換するための関数

bitlenmax :: Word8
bitlenmax = 64

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

-- type (ParseConditionSymbol a) => CacheFuncs state result = ((a -> Word64 -> state -> state),(state -> Maybe result))
-- data (ParseConditionSymbol a) => CacheFuncs state result = ((a -> Word64 -> state -> state),(state -> Maybe result))

class (Eq a,Enum a,Bounded a) => ParseConditionSymbol a where
  -- please define following functions --
  getLen    :: a -> Word8
  -- initCache :: ((a -> Word64 -> state -> state) , (state -> Maybe result))
  ---------- 

---  parse :: (EmptyExist state) => (a -> Word64 -> state -> state) -> (state -> Maybe result) -> ParseCondition a state result

  conditionDefines :: [(a,Word8)]
  conditionDefines = map (\sym -> (sym, getLen sym)) allSymbols

  firstCondition :: (EmptyExist state) => (a -> Word64 -> state -> state) -> (state -> Maybe result) -> ParseCondition a state result
  firstCondition f_StateUpdater f_StateToResult =
    let cache = MkCacheInfo mkEmpty (MkStateUpdater f_StateUpdater) (MkStateToResult f_StateToResult)
    in findParseCondition (Just minBound) cache

  allSymbols :: [a]
  allSymbols = enumFrom minBound
  
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

instance EmptyExist ByteHead where
  mkEmpty = (0,0)

instance EmptyExist Word64 where
  mkEmpty = 0

_readValue :: (FromWord64 a) => ValueCache -> BitLen -> (ByteHead,ByteString) -> (a,(ByteHead,ByteString))
_readValue curr 0 x = (fromWord64 curr, x) -- キャッシュしていた値を期待する値に変換して正常終了

 -- 先頭部分のWord8の読み取りが終了した場合
_readValue curr len ((head,0),bytes) =
  case Data.ByteString.uncons bytes of
    -- 残りのデータが空の場合
    Nothing              -> _readValue curr (len-1) ((0,8),bytes) -- バイト配列はもう終わりなのに、まだ読み取らないといけない場合、残りは全部0であったとみなして処理を続行
    -- 残りのデータがある場合
    Just (head2, bytes2) -> _readValue curr len ((head2,8),bytes2) -- まだバイト配列にWord8が残っているので、先頭部分のWord8を入れ替えて続行

-- まだ必要な分が終わっておらず、かつ先頭部分のWord8が読み取りが終わっていない場合
_readValue curr len ((head,rest),bytes) =
  let shiftCnt = (min len rest) :: Word8  -- shiftする数は残りの読み取るべき数と先頭部分のWord8の残りとのどちらか少ない方に合わせる
      shiftCntInt = (fromInteger $ toInteger shiftCnt) :: Int 
      plus = ((fromInteger . toInteger) $ (head `shiftR` (8 - shiftCntInt))) :: Word64 -- -- 例えば3ビット分を転写する場合、右に5ビット移動させることで左3ビットが示す値を作成する
      newcurr = (curr `shiftL` shiftCntInt) + plus -- 例えば3ビット分を転写する場合、currを左に3ビット移動させて、空いた右3ビットにheadの左3ビットを転写する
      newhead = (head `shiftL` shiftCntInt)
      newrest = rest - shiftCnt
      newlen  = len  - shiftCnt
  in _readValue newcurr newlen ((newhead,newrest),bytes)  -- 先頭のWord8の左側のビット値を読み取ってWord64に転写する
                 
class FromWord64 a where
  fromWord64 :: Word64 -> a
  fromNothing :: a

  fromMaybeWord64 :: Maybe Word64 -> a
  fromMaybeWord64 (Just x) = fromWord64 x
  fromMaybeWord64 Nothing = fromNothing
  
  readValue :: BitLen -> (ByteHead,ByteString) -> (a,(ByteHead,ByteString))
  readValue bitlen x
    | bitlen > bitlenmax = readValue bitlenmax x -- if bitlen is greater than bitlenmax, bitlen is treated as bitlenmax
    | bitlen == 0 = (fromWord64 0,x) -- if bitlen is 0, do nothing
    | otherwise = _readValue mkEmpty bitlen x
  startReadValue :: BitLen -> ByteString -> (a,(ByteHead,ByteString))
  startReadValue bitlen bytes = readValue bitlen (mkEmpty,bytes)
  
instance FromWord64 Bool where
  fromWord64 = (> 0) . (.&. 0x01)
  fromNothing = False
  
instance FromWord64 Word8 where
  fromWord64 = fromInteger . toInteger . (.&. 0xFF)
  fromNothing = 0

instance FromWord64 Char where
  fromWord64 = chr . fromInteger . toInteger . (.&. 0xFF)  
  fromNothing = '\0'
  
instance FromWord64 Word16 where
  fromWord64 = fromInteger . toInteger . (.&. 0xFFFF)
  fromNothing = 0
  
instance FromWord64 Word32 where
  fromWord64 = fromInteger . toInteger . (.&. 0xFFFFFFFF)
  fromNothing = 0
  
instance FromWord64 Word64 where
  fromWord64 x = x
  fromNothing = 0

data ParseFailedInfo = DataIsTooShort Word64 | UnknownReason
  
data ParseResult a = Parsed a (ByteHead,ByteString) | MaybeBug | NotMatch | ParseFailed ParseFailedInfo 

class HasParser a where
  startParse :: (ParseConditionSymbol sym,EmptyExist state) => ByteString -> (sym -> Word64 -> state -> state) -> (state -> Maybe a) -> ParseResult a
  startParse bytes f1 f2 = parse (mkEmpty, bytes) (firstCondition f1 f2)
  parse :: (Eq sym,EmptyExist st) => (ByteHead,ByteString) -> (ParseCondition sym st a) -> ParseResult a
  parse bytes (ParseStart cond) = parse bytes cond -- パース開始
  parse bytes (NextParse len sym state next) =
    let res = readValue len bytes
        val = fromWord64 $ fst res
        rest = snd res
    in parse rest (next val sym state)
  parse bytes (ParseFinished mres) = case mres of
    Nothing -> ParseFailed UnknownReason
    Just res -> Parsed res bytes -- 終了  

