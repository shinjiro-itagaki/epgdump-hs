{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser (
  ParseResult(..)
  ,HasParser(..)
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

instance EmptyExist Char where
  mkEmpty = '\0'
  
instance EmptyExist Bool where
  mkEmpty = False
  
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
  
instance FromWord64 Word64 where
  fromWord64 x = x

data ParseResult a =
  Parsed a
  | DataIsTooShort (Maybe BytesLen)-- 一致するデータがあったが、元データが不足している（続きのデータを追加して再実行すればうまくいくと思われる）。値は不足しているバイト数
  | NotMatch       -- 一致するデータが存在しなかった
  | UnknownReason  -- 原因不明の失敗

data (BytesHolderIO bh, HasParser result) => ParseIOFlow bh result =
  MkFlowStart
  | MkParseIOFlow (bh -> result -> IO (ParseResult result, bh))
  | MkParseIOFlowPair (ParseIOFlow bh result) (ParseIOFlow bh result)
  | MkFlowEnd

class (EmptyExist a) => HasParser a where
  -- please implement
  parseIOFlow :: (BytesHolderIO bh) => ParseIOFlow bh a
  -----

  mapParseResult :: (HasParser b) => ParseResult a -> b -> ParseResult b
  mapParseResult x y = case x of
    Parsed _         -> Parsed y
    DataIsTooShort x -> DataIsTooShort x
    NotMatch         -> NotMatch 
    UnknownReason    -> UnknownReason


  (|>>=) :: (BytesHolderIO bh) => ParseIOFlow bh a -> (bh -> a -> IO (ParseResult a, bh)) -> ParseIOFlow bh a
  (|>>=) flow func = MkParseIOFlowPair flow (MkParseIOFlow func)
  infixl 4 |>>=
    
  (=<<|) :: (BytesHolderIO bh) => (bh -> a -> IO (ParseResult a, bh)) -> ParseIOFlow bh a -> ParseIOFlow bh a
  (=<<|) func flow = flow |>>= func
  infixr 3 =<<|

  flowStart :: ParseIOFlow bh a
  flowStart = MkFlowStart

  parseIO :: (BytesHolderIO bh) => bh -> IO (ParseResult a, bh)
  parseIO bh = execParseIOFlow bh mkEmpty parseIOFlow

  execParseIOFlow :: (BytesHolderIO bh) => bh -> a -> ParseIOFlow bh a -> IO (ParseResult a, bh)
  execParseIOFlow bh init  MkFlowStart = return (Parsed init, bh) -- 何もせずそのまま成功として返す
  execParseIOFlow bh init  MkFlowEnd   = return (Parsed init, bh)  
  execParseIOFlow bh init (MkParseIOFlow f) = f bh init
  execParseIOFlow bh init (MkParseIOFlowPair MkFlowEnd _) = execParseIOFlow bh init MkFlowEnd -- 右側は実行せずに無視する
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
