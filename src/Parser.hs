{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser (
  ByteHead
  ,ValueCache
  ,CacheInfo
  ,cacheToResult
  ,ParseCondition
  ,ParseConditionSymbol(..)
  ,FromValueCache(..)
  ,ParseResult(..)
  ,HasParser(..)
  ,Parser.or
  ,FromWord64(..)
) where

import Common(EmptyExist(..),BytesLen,BitsLen,BytesHolderIO(..))
import Data.Int(Int64)
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Lazy(ByteString)
import Data.ByteString.Lazy as BS
import Data.Bits((.&.),(.|.),shiftL,shiftR,finiteBitSize)
import Data.Char(chr)
import Data.Maybe(fromMaybe)
import Control.Applicative((<|>))
import qualified Data.Vector as V
import qualified TS.FileHandle as FileHandle

type ByteHead = Word8
type ByteRest = ByteString

data EnableBits a = All | LeftBits a

instance EmptyExist (EnableBits a) where
  mkEmpty = All

-- ByteStringをunpackした後の型の桁数を返す

w8lenInt :: Int
w8lenInt = finiteBitSize $ Prelude.head $ (++[0]) $ BS.unpack BS.empty 

w8len :: BitsLen
w8len = fromInteger $ toInteger $ w8lenInt

toEnableBits :: (Integral a) => a -> EnableBits BitsLen
toEnableBits n
  | (1 < (toInteger n)) && ((toInteger n) < (toInteger w8len)) = LeftBits $ fromInteger $ toInteger n
  | otherwise = All

toNum :: (Num a) => EnableBits BitsLen -> a
toNum x = fromInteger
  $ case x of
      All          -> toInteger $ w8len
      (LeftBits n) -> toInteger $ n

-- タプルの右側は先頭バイトの左側のうち、いくつが使われているかを示す値で、Allはすべてを意味する
type ValueCache = (ByteString,EnableBits BitsLen)

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
        tail8 = Prelude.map (fromInteger . toInteger) $ Prelude.reverse $ Prelude.take 8 $ Prelude.reverse $ ([0,0,0,0,0,0,0,0] ++) $ BS.unpack bytes
        head7 = Prelude.take 7 tail8
        last1 = Prelude.last tail8
        init = 0 :: Word64
        func rtn e = (rtn `shiftL` (finiteBitSize e)) .|. (fromInteger $  toInteger e)
    in case n of
      0 -> (Prelude.foldl func init tail8)
      _ -> (.|. (0xFF .&. last1)) $ (`shiftL` n) $ (Prelude.foldl func init head7) 

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
    BitsLen -- 取得するビットの長さ
    symbol -- 対象になっている項目を示す何か
    (CacheInfo symbol state result) -- キャッシュの内容一式
    (ValueCache -> symbol -> (CacheInfo symbol state result) -> ParseCondition symbol state result) -- パースした結果を受け取り、対象になっている項目、状態、を入力すると次のパース条件を返す関数

class (Eq a,Enum a,Bounded a) => ParseConditionSymbol a where
  -- please define following functions --
  getLen    :: a -> BitsLen
  ----------

  bitsLength :: [a] -> BitsLen
  bitsLength xs = Prelude.foldl (+) 0 $ Prelude.map getLen xs

  conditionDefines :: [(a,BitsLen)]
  conditionDefines = Prelude.map (\sym -> (sym, getLen sym)) allSymbols

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
      updated = updaterf sym rawv state -- hoge
      newstate = fst $ updated

  nextConditionGenerator :: (EmptyExist state) => ValueCache -> a -> CacheInfo a state result -> ParseCondition a state result
  nextConditionGenerator = _generateNextCondition findParseCondition

  findParseCondition :: (EmptyExist state) => Maybe a -> ValueCache -> CacheInfo a state result -> ParseCondition a state result
  findParseCondition Nothing    oldv cache = ParseFinished $ cacheToResult cache
  findParseCondition (Just sym) oldv cache = findParseConditionImpl sym oldv cache conditionDefines

  findParseConditionImpl :: (EmptyExist state) => a -> ValueCache -> CacheInfo a state result -> [(a,BitsLen)] -> ParseCondition a state result
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

data ReadValueResult a = ReadValueSuccess a | TooShort (Maybe BytesLen)
  
readValue :: BitsLen -> ValueCache -> ByteString -> ReadValueResult (ValueCache,ByteString)
readValue 0 oldv bytes = ReadValueSuccess (oldv,bytes)
readValue l (old,en) bytes =
  case en of
    All ->
      let (divn,modn) = (l `divMod`) $ fromInteger $ toInteger w8len
          n = fromInteger $ toInteger $ divn + 1
          -- bytes2 = bytes `Data.ByteString.Lazy.append` (Data.ByteString.Lazy.pack $ map (\x -> 0) [1..n]) -- 確実に長さがn以上のバイト配列を作成
          head = BS.take n bytes
          tail = BS.drop n bytes -- こちらはオリジナルのtailを取得
      in
          if (BS.length head) < n then TooShort (Just $ fromInteger $ toInteger $ n - (BS.length head) ) else ReadValueSuccess ((head,toEnableBits modn),tail)
    LeftBits n ->
      let n' = l + n
          (divn',modn') = n' `divMod` w8len
      in
        if divn' < 1
        then ReadValueSuccess ((old,toEnableBits n'),bytes) -- シークせずに有効ビット数のみ変更して終了
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

class FromWord64 a where
  fromWord64 :: Word64 -> a

instance FromWord64 Char where
  fromWord64 x = chr $ fromInteger $ toInteger ((fromWord64 x) :: Word8)
  
instance FromWord64 Bool where
  fromWord64 = (> 0)

instance FromWord64 Word8 where
  fromWord64 = fromInteger . toInteger . (.&. 0xFF)

instance FromWord64 Word16 where
  fromWord64 = fromInteger . toInteger . (.&. 0xFFFF)

instance FromWord64 Word32 where
  fromWord64 = fromInteger . toInteger . (.&. 0xFFFFFFFF)

data ParseResult a =
  Parsed a
  | DataIsTooShort (Maybe BytesLen)-- 一致するデータがあったが、元データが不足している（続きのデータを追加して再実行すればうまくいくと思われる）。値は不足しているバイト数
  | NotMatch       -- 一致するデータが存在しなかった
  | UnknownReason  -- 原因不明の失敗

-- data ParseResultIO a b =
--   ParsedIO a b
--   | DataIsTooShortIO (Maybe BytesLen) b -- 一致するデータがあったが、元データが不足している（続きのデータを追加して再実行すればうまくいくと思われる）。値は不足しているバイト数
--   | NotMatchIO b      -- 一致するデータが存在しなかった
--   | UnknownReasonIO b -- 原因不明の失敗

-- class FromWord64 a where
--   fromWord64 :: Word64 -> a
--           parse fh >>> (\(x,v) -> x { _x1 = v })
-- |>>= parser  (\(x,v) -> x { _header1 = v })
-- |>>= bits 16 (\(x,v) -> x { _x2 = v })
-- |>>= parser  (\(x,v) -> x { _header2 = v })
-- |>>= bits 16 (\(x,v) -> x { _x3 = v })
-- |>>= bits  8 (\(x,v) -> x { _x4 = v })
-- |>>= parser  (\(x,v) -> x { _x5 = v })
-- |>>= branch  (\x ->
--                  if flag x
--                     then (bits 16 (\(x,v) -> x { _x6 = v }))
--                     else (bits 16 (\(x,v) -> x { _x7 = v }))
--              )
-- |>>= ...
-- data (BytesHolderIO bh, HasParser sub) => ParseFlowIO bh result =
--   ByBits BitsLen ((Word64,result) -> result) -- getBitsで値を作成
--   | ByParse      ((sub,   result) -> result) -- parseIOで値を作成
--   | PairFlow (ParseFlowIO bh result) (ParseFlowIO bh result)
--   | Branch (result -> ParseFlowIO bh result)
--   | Finished -- 終了フラグ

-- bits bl f = ByBits bl f
-- parser f = ByParse f

-- (|>>=) l r = PairFlow l r
-- infixl 4 |>>=

-- (=<<|) l r = PairFlow r l
-- infixl 3 =<<|

or :: ParseResult a -> a -> a
or (Parsed x) y = x
or _ y = y

data (BytesHolderIO bh, HasParser result) => ParseIOFlow bh result =
  MkParseIOFlow (bh -> result -> IO (ParseResult result, bh))
  | MkParseIOFlowPair (ParseIOFlow bh result) (ParseIOFlow bh result)
  | Finish

(|>>=) :: (BytesHolderIO bh, HasParser result) => ParseIOFlow bh result -> ParseIOFlow bh result -> ParseIOFlow bh result
(|>>=) c1 c2 = MkParseIOFlowPair c1 c2
infixl 4 |>>=

(=<<|) l r = r |>>= l
infixl 3 =<<|

class (EmptyExist a) => HasParser a where
  -- please implement
  -- startParse に関数を2つ追加すれば実装できる
  -- ex. parse = startParse update result
  parse :: ByteString -> (ParseResult a, ByteString)
  parseIOFlow :: (BytesHolderIO bh) => ParseIOFlow bh a
  -----

  flowStart :: (BytesHolderIO bh) => (bh -> a -> IO (ParseResult a, bh)) -> ParseIOFlow bh a
  flowStart = MkParseIOFlow

  parseIO :: (BytesHolderIO bh) => bh -> IO (ParseResult a, bh)
  parseIO bh = execParseIOFlow bh mkEmpty parseIOFlow

  execParseIOFlow :: (BytesHolderIO bh) => bh -> a -> ParseIOFlow bh a -> IO (ParseResult a, bh)
  execParseIOFlow bh init (MkParseIOFlow f) = f bh init
  execParseIOFlow bh init (MkParseIOFlowPair l r) = do
    lres@(res, bh2) <- execParseIOFlow bh init l
    case res of
      Parsed init2 -> execParseIOFlow bh2 init2 r
      x -> return lres
    
  -- 
  getBitsIO_M :: (BytesHolderIO bh) => bh -> [(BitsLen, (Word64,a) -> a)] -> a -> IO (ParseResult a, bh)
  getBitsIO_M fh conds init = do
    Prelude.foldl each' (return (Parsed init,fh)) conds
    where
      each' rtn (i,f) = do
        res@(res_d,fh') <- rtn
        case res_d of
          Parsed d -> do
            (v,fh'') <- getBitsIO fh' i 
            return (Parsed $ f (v,d), fh'')
          DataIsTooShort mblen -> return $ (\x->(x,fh')) $ DataIsTooShort $ Just $ (+i) $ fromMaybe 0 mblen
          x -> return res

    
  
--  update :: (BytesHolderIO bh) => a -> bh -> IO (a, bh)

  -- パースした結果と余ったByteString
  startParse :: (ParseConditionSymbol sym,EmptyExist state) => (sym -> ValueCache -> state -> (state,Maybe sym)) -> (state -> Maybe a) -> ByteString -> (ParseResult a,ByteString)
  startParse f1 f2 bytes = parse__ bytes (firstCondition f1 f2)
  parse__ :: (Eq sym,EmptyExist st) => ByteString -> (ParseCondition sym st a) -> (ParseResult a,ByteString)
  parse__ bytes (ParseStart cond) = parse__ bytes cond -- パース開始
  parse__ bytes (NextParse oldv len sym state next) =
    let res = readValue len oldv bytes
    in case res of
      ReadValueSuccess (val,rest) -> parse__ rest (next val sym state)
      TooShort x -> (DataIsTooShort x,BS.empty)
  parse__ bytes (ParseFinished mres) = (\x -> (x,bytes)) $ case mres of
    Nothing -> UnknownReason
    Just res -> Parsed res -- 終了

  parseMulti' :: V.Vector a -> ByteString -> (V.Vector a, ByteString)
  parseMulti' curr bytes =
    case parse bytes of
      (Parsed a        ,rest) -> let next = parseMulti' (V.snoc curr a) rest in (fst next, snd next)
      (DataIsTooShort _,rest) -> (curr,rest) -- データが短すぎて終了したのでこれで終わり
      (_               ,rest) -> parseMulti' curr rest -- 失敗したようだが、データは残っていると思われるので続行

  parseMulti :: ByteString -> (V.Vector a, ByteString)
  parseMulti = parseMulti' V.empty
