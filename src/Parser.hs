{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser (
  ByteLen
  ,ByteHead
  ,ValueCache
  ,CacheInfo
  ,cacheToResult
  ,ParseCondition
  ,ParseConditionSymbol(..)
  ,FromValueCache(..)
  ,ParseFailedInfo(..)
  ,ParseResult(..)
  ,HasParser(..)
) where

import Common(EmptyExist(..))
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString(ByteString,pack,unpack,null,uncons,empty,take,drop,append)
import Data.Bits((.&.),(.|.),shiftL,shiftR,finiteBitSize)
import Data.Char(chr)

type ByteLen = Word8
type ByteHead = Word8
type ByteRest = ByteString
-- type ValueCache = (Word64,ByteString)
type ValueCache = ByteString

class ToWord a where
  toWord64 :: a -> Word64
  toWord32 :: a -> Word32
  toWord16 :: a -> Word16
  toWord8  :: a -> Word8
  toWord32 = fromInteger . toInteger . (.&. 0xFFFFFFFF) . toWord64
  toWord16 = fromInteger . toInteger . (.&. 0xFFFF)     . toWord64
  toWord8  = fromInteger . toInteger . (.&. 0xFF)       . toWord64
  
instance ToWord ByteString where
  toWord64 x =
    let  tail4 = Prelude.reverse $ Prelude.take 4 $ Prelude.reverse $ ([0,0,0,0] ++) $ unpack x
         init = 0 :: Word64
         func rtn e = (rtn `shiftL` (finiteBitSize e)) .|. (fromInteger $  toInteger e)
    in foldl func init tail4

data (Eq sym, EmptyExist state) => StateUpdater sym state = MkStateUpdater (sym -> ValueCache -> state -> state)
data (EmptyExist state) => StateToResult state result = MkStateToResult (state -> Maybe result)

data (EmptyExist state) => CacheInfo sym state result = MkCacheInfo state (StateUpdater sym state) (StateToResult state result) -- 状態の具体的なデータ、状態を更新する関数 状態を最終結果に変換するための関数

cacheToResult :: (EmptyExist state) => (CacheInfo sym state result) -> Maybe result
cacheToResult (MkCacheInfo st _ (MkStateToResult f)) = f st

data (Eq symbol, EmptyExist state) => ParseCondition symbol state result =
  ParseStart (ParseCondition symbol state result) -- 一番最初のパース条件を返す関数を含んだコンストラクタ
  | ParseFinished (Maybe result) -- パースを終了して状態から成果物を作成するためのコンストラクタ
  | NextParse
    ByteLen -- 取得するビットの長さ  
    symbol -- 対象になっている項目を示す何か
    (CacheInfo symbol state result) -- キャッシュの内容一式
    (ValueCache -> symbol -> (CacheInfo symbol state result) -> ParseCondition symbol state result) -- パースした結果を受け取り、対象になっている項目、状態、を入力すると次のパース条件を返す関数

class (Eq a,Enum a,Bounded a) => ParseConditionSymbol a where
  -- please define following functions --
  getLen    :: a -> ByteLen
  ---------- 

  conditionDefines :: [(a,ByteLen)]
  conditionDefines = map (\sym -> (sym, getLen sym)) allSymbols

  firstCondition :: (EmptyExist state) => (a -> ValueCache -> state -> state) -> (state -> Maybe result) -> ParseCondition a state result
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

  _generateNextCondition :: (EmptyExist state) => (Maybe a -> CacheInfo a state result -> ParseCondition a state result) -> ValueCache -> a -> CacheInfo a state result -> ParseCondition a state result
  _generateNextCondition cond_finder rawv sym (MkCacheInfo state updater@(MkStateUpdater updaterf) fx) = cond_finder nextsym (MkCacheInfo newstate updater fx)
    where
      nextsym = findNext sym
      newstate = updaterf sym rawv state

  nextConditionGenerator :: (EmptyExist state) => ValueCache -> a -> CacheInfo a state result -> ParseCondition a state result
  nextConditionGenerator = _generateNextCondition findParseCondition

  findParseCondition :: (EmptyExist state) => Maybe a -> CacheInfo a state result -> ParseCondition a state result
  findParseCondition Nothing    cache = ParseFinished $ cacheToResult cache
  findParseCondition (Just sym) cache = findParseConditionImpl sym cache conditionDefines

  findParseConditionImpl :: (EmptyExist state) => a -> CacheInfo a state result -> [(a,ByteLen)] -> ParseCondition a state result
  findParseConditionImpl   _ cache     [] = ParseFinished $ cacheToResult cache
  findParseConditionImpl sym cache (x:[]) = ParseFinished $ cacheToResult cache
  findParseConditionImpl sym cache ((xsym,xlen):xs)
    | sym == xsym = NextParse xlen xsym cache nextConditionGenerator
    | otherwise = findParseConditionImpl sym cache xs

--instance EmptyExist ValueCache where
--  mkEmpty = (mkEmpty,mkEmpty)

--_readValue :: (FromValueCache a) => ByteLen -> ByteString -> ((a,ByteString),ByteString)
--_readValue 0 rest = (fromValueCache curr, rest) -- キャッシュしていた値を期待する値に変換して正常終了
-- _readValue l rest =
--   let headbitlen  = fromInteger $ toInteger $ finiteBitSize head
--   in 
--   case Data.ByteString.uncons bytes_rest of
--     -- 残りのデータが空の場合
--     Nothing -> _readValue curr (l-1) (mkEmpty,bytes_rest) -- バイト配列はもう終わりなのに、まだ読み取らないといけない場合、残りは全部0であったとみなして処理を続行
--     -- 残りのデータがある場合
--     Just x  -> _readValue curr l x -- まだバイト配列にWord8が残っているので、先頭部分のWord8を入れ替えて続行

-- -- まだ必要な分が終わっておらず、かつ先頭部分のWord8が読み取りが終わっていない場合
-- _readValue (curr,currbytes) len ((head,rest),bytes) =
--   let headbitlen = finiteBitSize head in  
--   let shiftCnt = (min len rest) :: ByteLen  -- shiftする数は残りの読み取るべき数と先頭部分のWord8の残りとのどちらか少ない方に合わせる
--       shiftCntInt = (fromInteger $ toInteger shiftCnt) :: Int 
--       plus = ((fromInteger . toInteger) $ (head `shiftR` (headbitlen - shiftCntInt))) :: Word64 -- -- 例えば3ビット分を転写する場合、右に5ビット移動させることで左3ビットが示す値を作成する
--       newcurr = (curr `shiftL` shiftCntInt) + plus -- 例えば3ビット分を転写する場合、currを左に3ビット移動させて、空いた右3ビットにheadの左3ビットを転写する
--       newhead = (head `shiftL` shiftCntInt)
--       newrest = rest - shiftCnt
--       newlen  = len  - shiftCnt
--   in _readValue (newcurr,currbytes) newlen ((newhead,newrest),bytes)  -- 先頭のWord8の左側のビット値を読み取ってValueCacheに転写する
                 
class (EmptyExist a) => FromValueCache a where
  fromValueCache :: ValueCache -> a
  fromNothing :: a
  fromNothing = mkEmpty

  fromMaybeValueCache :: Maybe ValueCache -> a
  fromMaybeValueCache (Just x) = fromValueCache x
  fromMaybeValueCache Nothing = fromNothing

  
readValue :: ByteLen -> ValueCache -> (ValueCache,ByteString)
readValue 0   bytes = (mkEmpty,bytes)
readValue len bytes =
  let n = fromInteger $ toInteger len
      bytes2 = bytes `Data.ByteString.append` (Data.ByteString.pack $ map (\x -> 0) [1..n])
      head = Data.ByteString.take n bytes2
      tail = Data.ByteString.drop n bytes2
  in  (head,tail)
  
instance FromValueCache Bool where
  fromValueCache = (> 0) . (.&. 0x01) . toWord8
  
instance FromValueCache Word8 where
  fromValueCache = fromInteger . toInteger . (.&. 0xFF) . toWord8

instance EmptyExist Char where
  mkEmpty = '\0'
  
instance EmptyExist Bool where
  mkEmpty = False
  
instance FromValueCache Char where
  fromValueCache = chr . fromInteger . toInteger . (.&. 0xFF) . toWord8
  
instance FromValueCache Word16 where
  fromValueCache = toWord16
  
instance FromValueCache Word32 where
  fromValueCache = toWord32
  
instance FromValueCache Word64 where
  fromValueCache = toWord64

instance FromValueCache ValueCache where
  fromValueCache x = x

data ParseFailedInfo = DataIsTooShort ValueCache | UnknownReason
  
data ParseResult a = Parsed a ByteString | MaybeBug | NotMatch | ParseFailed ParseFailedInfo 

class HasParser a where
  startParse :: (ParseConditionSymbol sym,EmptyExist state) => ByteString -> (sym -> ValueCache -> state -> state) -> (state -> Maybe a) -> ParseResult a
  startParse bytes f1 f2 = parse bytes (firstCondition f1 f2)
  parse :: (Eq sym,EmptyExist st) => ByteString -> (ParseCondition sym st a) -> ParseResult a
  parse bytes (ParseStart cond) = parse bytes cond -- パース開始
  parse bytes (NextParse len sym state next) =
    let res = readValue len bytes
        val = fst res
        rest = snd res
    in parse rest (next val sym state)
  parse bytes (ParseFinished mres) = case mres of
    Nothing -> ParseFailed UnknownReason
    Just res -> Parsed res bytes -- 終了  
