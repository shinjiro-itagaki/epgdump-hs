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
  adaptation_field :: t -> Maybe AdaptationField.Data
  payload          :: t -> ByteString
  (===) :: t -> t -> Bool
  duplicated :: t -> Bool
  setDuplicated :: t -> t
  --

  header :: t -> Header.Data
  header = Header.header

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
  _payload :: Payload,
  _duplicated :: Bool
  } | EOF | ContinuityCounterError deriving(Eq)

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty (Just AdaptationField.mkEmpty) mkEmpty False

instance Header.Class Data where
  header x@(MkData _ _ _ _) = _header x
  header  _             = mkEmpty :: Header.Data  
  
instance Class Data where
  (===) x y = x == y
  
  adaptation_field x@(MkData _ _ _ _) = _maf x
  adaptation_field _                  = Nothing

  payload x@(MkData _ _ _ _) = _payload x
  payload _                  = BS.empty
  
  isEOF EOF = True
  isEOF _   = False
  
  isOK (MkData _ _ _ _) = True
  isOK _                = False

  mkEOF = EOF
  mkOK h af b = MkData h af b False

  duplicated x@(MkData _ _ _ _) = _duplicated x
  duplicated _                  = False

  setDuplicated (MkData a b c d) = (MkData a b c True)
  setDuplicated x                = x
  
