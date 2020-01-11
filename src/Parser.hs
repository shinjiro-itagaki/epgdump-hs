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
  ,Parser.or
  ,BitLen
) where

import Common(EmptyExist(..))
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString(ByteString,pack,unpack,null,uncons,empty,take,drop,append)
import Data.Bits((.&.),(.|.),shiftL,shiftR,finiteBitSize)
import Data.Char(chr)
import Data.Maybe(fromMaybe)
import Control.Applicative((<|>))

type ByteLen = Word8
type BitLen  = Word8
type ByteHead = Word8
type ByteRest = ByteString

data EnableBits a = All | LeftBits a

instance EmptyExist (EnableBits a) where
  mkEmpty = All

-- Data.ByteStringをunpackした後の型の桁数を返す

w8lenInt :: Int
w8lenInt = finiteBitSize $ head $ (++[0]) $ unpack $ Data.ByteString.empty 

w8len :: BitLen
w8len = fromInteger $ toInteger $ w8lenInt

toEnableBits :: (Integral a) => a -> EnableBits BitLen
toEnableBits n
  | (1 < (toInteger n)) && ((toInteger n) < (toInteger w8len)) = LeftBits $ fromInteger $ toInteger n
  | otherwise = All

toNum :: (Num a) => EnableBits BitLen -> a
toNum x = fromInteger
  $ case x of
      All          -> toInteger $ w8len
      (LeftBits n) -> toInteger $ n

-- タプルの右側は先頭バイトの左側のうち、いくつが使われているかを示す値で、Allはすべてを意味する
type ValueCache = (ByteString,EnableBits BitLen)

instance EmptyExist ValueCache where
  mkEmpty = (mkEmpty,mkEmpty)

class ToWord a where
  toWord64 :: a -> Word64
  toWord32 :: a -> Word32
  toWord16 :: a -> Word16
  toWord8  :: a -> Word8
  toWord32 = fromInteger . toInteger . (.&. 0xFFFFFFFF) . toWord64
  toWord16 = fromInteger . toInteger . (.&. 0xFFFF)     . toWord64
  toWord8  = fromInteger . toInteger . (.&. 0xFF)       . toWord64

instance ToWord ValueCache where
  toWord64 (bytes,enable) =
    let n = max 0 $ min w8lenInt $ toNum enable -- 0 <= n <= 7 :: Int
        tail8 = map (fromInteger . toInteger) $ Prelude.reverse $ Prelude.take 8 $ Prelude.reverse $ ([0,0,0,0,0,0,0,0] ++) $ unpack bytes
        head7 = Prelude.take 7 tail8
        last1 = Prelude.last tail8
        init = 0 :: Word64
        func rtn e = (rtn `shiftL` (finiteBitSize e)) .|. (fromInteger $  toInteger e)
    in case n of
      0 -> (foldl func init tail8)
      _ -> (.|. (0xFF .&. last1)) $ (`shiftL` n) $ (foldl func init head7) 

instance ToWord ByteString where
  toWord64 x = toWord64 ((x,mkEmpty) :: ValueCache)
  
data (Eq sym, EmptyExist state) => StateUpdater sym state = MkStateUpdater (sym -> ValueCache -> state -> (state,Maybe sym))
data (EmptyExist state) => StateToResult state result = MkStateToResult (state -> Maybe result)

data (EmptyExist state) => CacheInfo sym state result = MkCacheInfo state (StateUpdater sym state) (StateToResult state result) -- 状態の具体的なデータ、状態を更新する関数 状態を最終結果に変換するための関数

cacheToResult :: (EmptyExist state) => (CacheInfo sym state result) -> Maybe result
cacheToResult (MkCacheInfo st _ (MkStateToResult f)) = f st

data (Eq symbol, EmptyExist state) => ParseCondition symbol state result =
  ParseStart (ParseCondition symbol state result) -- 一番最初のパース条件を返す関数を含んだコンストラクタ
  | ParseFinished (Maybe result) -- パースを終了して状態から成果物を作成するためのコンストラクタ
  | NextParse
    ValueCache -- 前回の値
    BitLen -- 取得するビットの長さ
    symbol -- 対象になっている項目を示す何か
    (CacheInfo symbol state result) -- キャッシュの内容一式
    (ValueCache -> symbol -> (CacheInfo symbol state result) -> ParseCondition symbol state result) -- パースした結果を受け取り、対象になっている項目、状態、を入力すると次のパース条件を返す関数

data BitLenType = Static BitLen | Dynamic

class (Eq a,Enum a,Bounded a) => ParseConditionSymbol a where
  -- please define following functions --
  getLen    :: a -> BitLen
  ---------- 

  bitLength :: [a] -> BitLen
  bitLength xs = foldl (+) 0 $ map getLen xs

  conditionDefines :: [(a,BitLen)]
  conditionDefines = map (\sym -> (sym, getLen sym)) allSymbols

  firstCondition :: (EmptyExist state) => (a -> ValueCache -> state -> (state,Maybe a)) -> (state -> Maybe result) -> ParseCondition a state result
  firstCondition f_StateUpdater f_StateToResult =
    let cache = MkCacheInfo mkEmpty (MkStateUpdater f_StateUpdater) (MkStateToResult f_StateToResult)
    in findParseCondition (Just minBound) mkEmpty cache

  allSymbols :: [a]
  allSymbols = enumFrom minBound
  
  findNext :: a -> Maybe a
  findNext sym = impl conditionDefines
   where
     impl []     = Nothing
     impl (x:[]) = Nothing
     impl (x:(y:ys)) = if (fst x) == sym then Just $ fst y else impl (y:ys)

  _generateNextCondition :: (EmptyExist state) => (Maybe a -> ValueCache -> CacheInfo a state result -> ParseCondition a state result) -> ValueCache -> a -> CacheInfo a state result -> ParseCondition a state result
  _generateNextCondition cond_finder rawv sym (MkCacheInfo state updater@(MkStateUpdater updaterf) fx) = cond_finder nextsym rawv (MkCacheInfo newstate updater fx)
    where
      nextsym = (snd updated) <|> (findNext sym)
      updated = updaterf sym rawv state
      newstate = fst $ updated

  nextConditionGenerator :: (EmptyExist state) => ValueCache -> a -> CacheInfo a state result -> ParseCondition a state result
  nextConditionGenerator = _generateNextCondition findParseCondition

  findParseCondition :: (EmptyExist state) => Maybe a -> ValueCache -> CacheInfo a state result -> ParseCondition a state result
  findParseCondition Nothing    oldv cache = ParseFinished $ cacheToResult cache
  findParseCondition (Just sym) oldv cache = findParseConditionImpl sym oldv cache conditionDefines

  findParseConditionImpl :: (EmptyExist state) => a -> ValueCache -> CacheInfo a state result -> [(a,BitLen)] -> ParseCondition a state result
  findParseConditionImpl   _ oldv cache     [] = ParseFinished $ cacheToResult cache
  findParseConditionImpl sym oldv cache (x:[]) = ParseFinished $ cacheToResult cache
  findParseConditionImpl sym oldv cache ((xsym,xbitlen):xs)
    | sym == xsym = NextParse oldv xbitlen xsym cache nextConditionGenerator
    | otherwise = findParseConditionImpl sym oldv cache xs

--  update :: a -> ValueCache -> b -> b

class (EmptyExist a) => FromValueCache a where
  fromValueCache :: ValueCache -> a
  fromNothing :: a
  fromNothing = mkEmpty

  fromMaybeValueCache :: Maybe ValueCache -> a
  fromMaybeValueCache (Just x) = fromValueCache x
  fromMaybeValueCache Nothing = fromNothing
  
readValue :: BitLen -> ValueCache -> ByteString -> (ValueCache,ByteString)
readValue 0 oldv    bytes = (oldv,bytes)
readValue l (old,en) bytes =
  case en of
    All ->
      let (divn,modn) = l `divMod` w8len
          n = (fromInteger $ toInteger $ divn + 1) :: Int
          bytes2 = bytes `Data.ByteString.append` (Data.ByteString.pack $ map (\x -> 0) [1..n]) -- 確実に長さがn以上のバイト配列を作成
          head = Data.ByteString.take n bytes2
          tail = Data.ByteString.drop n bytes2
      in
          ((head,toEnableBits modn),tail)
    LeftBits n ->
      let n' = l + n
          (divn',modn') = n' `divMod` w8len
      in
        if divn' < 1
        then ((old,toEnableBits n'),bytes) -- シークせずに有効ビット数のみ変更して終了
        else readValue divn' (old,All) bytes

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

or :: ParseResult a -> a -> a
or (Parsed x _) y = x
or _ y = y

class HasParser a where
  startParse :: (ParseConditionSymbol sym,EmptyExist state) => (sym -> ValueCache -> state -> (state,Maybe sym)) -> (state -> Maybe a) -> ByteString -> ParseResult a
  startParse f1 f2 bytes = parse__ bytes (firstCondition f1 f2)
  parse__ :: (Eq sym,EmptyExist st) => ByteString -> (ParseCondition sym st a) -> ParseResult a
  parse__ bytes (ParseStart cond) = parse__ bytes cond -- パース開始
  parse__ bytes (NextParse oldv len sym state next) =
    let (val,rest) = readValue len oldv bytes
    in parse__ rest (next val sym state)
  parse__ bytes (ParseFinished mres) = case mres of
    Nothing -> ParseFailed UnknownReason
    Just res -> Parsed res bytes -- 終了

  parse :: ByteString -> ParseResult a
