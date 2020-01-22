{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser where

import Common(EmptyExist(..),BytesLen,BitsLen)
import qualified BytesReader.Base as BytesReaderBase
import Data.Int(Int64)
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Lazy(ByteString)
import Data.ByteString.Lazy as BS
import Data.Maybe(fromMaybe)
import Control.Applicative((<|>))
import qualified Data.Vector as V
import qualified TS.FileHandle as FileHandle
import qualified Parser.Result as Result
import qualified FromWord64

instance EmptyExist Char where
  mkEmpty = '\0'
  
instance EmptyExist Bool where
  mkEmpty = False

type ParseResult = Result.Data

mapParseResult :: (Show a, Show b) => (a -> b) -> Result.Data a -> Result.Data b
mapParseResult = Result.map

data (BytesReaderBase.Class bh, Class result) => ParseIOFlow bh result =
  MkFlowStart
  | MkParseIOFlow (bh -> result -> IO (ParseResult result, bh))
  | MkParseIOFlowPair (ParseIOFlow bh result) (ParseIOFlow bh result)
  | MkFlowEnd

(>>==) :: (BytesReaderBase.Class bh, Class result) => ParseIOFlow bh result -> ParseIOFlow bh result -> ParseIOFlow bh result
(>>==) = MkParseIOFlowPair
infixl 2 >>==

(==<<) :: (BytesReaderBase.Class bh, Class result) => ParseIOFlow bh result -> ParseIOFlow bh result -> ParseIOFlow bh result  
(==<<) l r = r >>== l
infixl 2 ==<<
  
class (EmptyExist a, Show a) => Class a where
  -- please implement if you need
  parseIOFlow :: (BytesReaderBase.Class bh) => ParseIOFlow bh a
  parseIOFlow = flowStart
  -----

  (|>>=) :: (BytesReaderBase.Class bh) => ParseIOFlow bh a -> (bh -> a -> IO (ParseResult a, bh)) -> ParseIOFlow bh a
  (|>>=) flow func = MkParseIOFlowPair flow (MkParseIOFlow func)
  infixl 4 |>>=
    
  (=<<|) :: (BytesReaderBase.Class bh) => (bh -> a -> IO (ParseResult a, bh)) -> ParseIOFlow bh a -> ParseIOFlow bh a
  (=<<|) func flow = flow |>>= func
  infixr 3 =<<|

  flowStart :: ParseIOFlow bh a
  flowStart = MkFlowStart

  parseIO :: (BytesReaderBase.Class bh) => bh -> IO (ParseResult a, bh)
  parseIO bh = execParseIOFlow bh mkEmpty parseIOFlow

  execParseIOFlow :: (BytesReaderBase.Class bh) => bh -> a -> ParseIOFlow bh a -> IO (ParseResult a, bh)
  execParseIOFlow bh init  MkFlowStart = return (Result.Parsed init, bh) -- 何もせずそのまま成功として返す
  execParseIOFlow bh init  MkFlowEnd   = return (Result.Parsed init, bh)  
  execParseIOFlow bh init (MkParseIOFlow f) = f bh init
  execParseIOFlow bh init (MkParseIOFlowPair MkFlowEnd _) = execParseIOFlow bh init MkFlowEnd -- 右側は実行せずに無視する
  execParseIOFlow bh init (MkParseIOFlowPair l r) = do
    lres@(res, bh2) <- execParseIOFlow bh init l
    case res of
      Result.Parsed init2 -> execParseIOFlow bh2 init2 r
      x -> return lres
  -- 
  getBitsIO_M :: (BytesReaderBase.Class bh) => bh -> [(BitsLen, (Word64,a) -> a)] -> a -> IO (ParseResult a, bh)
  getBitsIO_M fh conds init = do
    Prelude.foldl each' (return (Result.Parsed init,fh)) conds
    where
      each' rtn (i,f) = do
        res@(res_d,fh') <- rtn
        case res_d of
          Result.Parsed d -> do
            (v,fh'') <- BytesReaderBase.getBitsIO fh' i 
            return (Result.Parsed $ f (v,d), fh'')
          Result.DataIsTooShort mblen -> return $ (\x->(x,fh')) $ Result.DataIsTooShort $ Just $ (+i) $ fromMaybe 0 mblen
          x -> return res

parseFlow :: (BytesReaderBase.Class bh, Class a, Class b) => (a -> b -> b) -> bh -> b -> IO (ParseResult b, bh)
parseFlow caster fh init = do
  (res_header,fh') <- parseIO fh
  return $ (\x -> (x,fh')) $ mapParseResult (\header1 -> caster header1 init) res_header
