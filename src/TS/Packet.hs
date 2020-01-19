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

class (Header.Class t) => Class t where
  -- please implement
  mkEOF :: t
  mkOK  :: Header.Data -> Maybe AdaptationField.Data -> Payload -> t
  isEOF :: t -> Bool
  isOK  :: t -> Bool
  body_data :: t -> ByteString
  (===) :: t -> t -> Bool
  duplicated :: t -> Bool
  setDuplicated :: t -> t
  --

  header :: t -> Header.Data
  header = Header.header
  
  adaptation_field :: t -> Maybe AdaptationField.Data
  payload          :: t -> ByteString
  -- payload x = BS.drop (payloadlen x) $ body_data x
  
  -- payloadlen :: (Num b) => t -> b
  -- payloadlen x = if Header.has_payload header' then selflen - adlen else 0
  --   where
  --     header' = header x
  --     selflen = fromInteger $ toInteger $ BS.length $ body_data x -- 自身の長さ
  --     adlen = case adaptation_field x of
  --               Just af -> AdaptationField.adaptation_field_length af
  --               Nothing -> 0


  -- 重複は一度だけ許可される
  -- body の discontinuity indicatorがtrueの場合は不連続でもよい
  continuityChecked :: t -> Maybe t -> Maybe t
  continuityChecked x my =
    case my of
      Nothing -> Just x
      Just y -> if x === y -- 同じパケットなので重複の可能性あり
                then if duplicated y -- 既に重複済みのものの場合
                     then Nothing -- ２回目の重複なので許可されない
                     else Just $ setDuplicated x -- 重複済みフラグを設定
                else Just x

  -- 引数のByteStringは同期用の先頭バイトは削られているものを想定している
  fromByteString :: ByteString -> Result.Data t
  fromByteString bytes =
    let plen'     = toInteger $ bytesLen - 1    :: Integer
        plen''    = fromInteger plen'           :: Int64
        byteslen' = toInteger $ BS.length bytes :: Integer
        bytes'    = BS.take plen'' bytes
        header    = Header.parse $ BS.take 3 bytes'
        (res,rest) = AdaptationField.parse $ BS.unpack $ BS.drop 3 bytes'
        body      = BS.pack rest
    in
      if byteslen' < plen'
      then Result.DataIsTooShort $ Just $ fromInteger $ (plen' - byteslen')
      else case res of
        Result.Parsed af        -> Result.Parsed $ mkOK header (Just af) body
        Result.DataIsTooShort i -> Result.DataIsTooShort i
        Result.NotMatch         -> Result.NotMatch
        Result.SumCheckError    -> Result.SumCheckError
        Result.UnknownReason    -> Result.UnknownReason

  read :: (FH.Class fh) => fh -> IO (Result.Data t,(BS.ByteString,fh))
  read h = do
    h' <- FH.syncIO h
    res@(bytes,h'') <- FH.getBytesIO h' (bytesLen - 1)
    return (fromByteString bytes,res)

data Data = MkData Header.Data (Maybe AdaptationField.Data) Payload | EOF | ContinuityCounterError

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty (Just AdaptationField.mkEmpty) mkEmpty

instance Header.Class Data where
  header (MkData h _ _) = h
  header  _             = mkEmpty :: Header.Data  
  
instance Class Data where
  body_data (MkData _ _ b) = b
  body_data _              = BS.empty

  adaptation_field (MkData _ af _) = af
  adaptation_field _ = Nothing
  
  isEOF EOF = True
  isEOF _   = False
  isOK (MkData _ _ _) = True
  isOK _              = False

  mkEOF = EOF
  mkOK h af b = MkData h af b
