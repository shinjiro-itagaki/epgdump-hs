{-# LANGUAGE FlexibleInstances #-}
module TS.Packet where

import qualified TS.Packet.Header as Header
--import qualified TS.FileHandle    as FH

import qualified BytesReader

import Data.Word(Word64, Word32, Word16, Word8)
import qualified Data.ByteString.Lazy as BS
import Common(BytesLen,EmptyExist(..),PID,TableID,ByteString)
import qualified Parser
import qualified Data.Vector as V
import Data.Int(Int64)
import qualified BytesReader.HolderIO as HolderIO
import qualified TS.FileHandle as FH
import qualified TS.Packet.AdaptationField as AdaptationField
import qualified Parser.Result as Result
-- sync byteを含めた長さ
bytesLen :: BytesLen
bytesLen = 188

type Payload = ByteString

data ContinuityType = Continuous | Duplicated | ContinuousErr

class (Header.Class t, Show t) => Class t where
  -- please implement
  mkEOF :: t
  mkOK  :: Header.Data -> Maybe AdaptationField.Data -> Payload -> t
  isEOF :: t -> Bool
  isOK  :: t -> Bool
  adaptation_field :: t -> Maybe AdaptationField.Data
  payload          :: t -> ByteString
  (===) :: t -> t -> Bool
  --

  header :: t -> Header.Data
  header = Header.header

  continuous :: t -> t -> Bool
  continuous x y =
    let res = (Header.next $ Header.continuity_counter x) == (Header.continuity_counter y)
    in case adaptation_field y of
      Just af -> if AdaptationField.discontinuity_indicator af
                 then True -- 不連続を許可するので常にtrue
                 else res
      Nothing -> res

  continuityChecked :: [t] -> [t]
  continuityChecked []       = []
  continuityChecked l@(x:xs) = if Header.payload_unit_start_indicator x then impl' [] l False else []
    where
      check' x y dup =
        if continuous x y then Continuous -- 問題なし
        else -- パケットの欠損が疑われる場合
          if x === y  -- 同じパケットなので重複の可能性あり
          then if dup
               then ContinuousErr -- ２回目の重複なので連続エラー
               else Duplicated -- 1回目の重複なので許可される
          else ContinuousErr -- 連続エラー
          
      impl' pre (x:[]) dup = pre ++ [x]
      impl' pre (x:(y:ys)) dup =
        case check' x y dup of
          Continuous    -> impl' (pre ++ [x]) (y:ys) False
          Duplicated    -> impl' (pre ++ [x]) ys     True -- 重複しているものは削除
          ContinuousErr -> []

  -- 引数のByteStringは同期用の先頭バイトは削られているものを想定している
  fromByteString :: ByteString -> Result.Data t
  fromByteString bytes =
    let plen'     = toInteger $ bytesLen - 1    :: Integer
        plen''    = fromInteger plen'           :: Int64
        byteslen' = toInteger $ BS.length bytes :: Integer
        bytes'    = BS.take plen'' bytes
        header    = Header.parse $ BS.take 3 bytes'
        (res,rest) = AdaptationField.parse header $ BS.unpack $ BS.drop 3 bytes'
        body      = BS.pack rest
    in
      if byteslen' < plen'
      then Result.DataIsTooShort $ Just $ fromInteger $ (plen' - byteslen')
      else case res of
        Result.Parsed maf       -> Result.Parsed $ mkOK header maf body
        Result.DataIsTooShort i -> Result.DataIsTooShort i
        Result.NotMatch         -> Result.NotMatch
        Result.SumCheckError    -> Result.SumCheckError
        Result.UnknownReason    -> Result.UnknownReason

  read :: (FH.Class fh) => fh -> IO (Result.Data t,(BS.ByteString,fh))
  read h = do
    h' <- FH.syncIO h
    isEOF' <- HolderIO.isEOF h'
    if isEOF'
      then return (Result.Parsed mkEOF, (BS.empty,h'))
      else do
      res@(bytes,h'') <- FH.getBytesIO h' (bytesLen - 1)
      return (fromByteString bytes,res)
      

data Data = MkData {
  _header  :: Header.Data,
  _maf     :: Maybe AdaptationField.Data,
  _payload :: Payload
  } | EOF | ContinuityCounterError deriving(Eq,Show)

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty (Just AdaptationField.mkEmpty) mkEmpty
  
instance Header.Class Data where
  header x@(MkData _ _ _) = _header x
  header  _               = mkEmpty :: Header.Data  
  
instance Class Data where
  (===) x y = x == y
  
  adaptation_field x@(MkData _ _ _) = _maf x
  adaptation_field _                = Nothing

  payload x@(MkData _ _ _) = _payload x
  payload _                = BS.empty
  
  isEOF EOF = True
  isEOF _   = False
  
  isOK (MkData _ _ _) = True
  isOK _              = False

  mkEOF = EOF
  mkOK h af b = MkData h af b
