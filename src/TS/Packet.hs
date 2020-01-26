{-# LANGUAGE FlexibleInstances #-}
module TS.Packet where

import qualified TS.Packet.Header as Header
--import qualified TS.FileHandle    as FH

import qualified BytesReader
import qualified Data.ByteString.Lazy as BS
import Utils
import qualified Utils.EmptyExist as EmptyExist
import qualified Data.Vector as V
import qualified BytesReader.Base as BytesReaderBase
import qualified TS.FileHandle as FH
import qualified TS.Packet.AdaptationField as AdaptationField
import qualified Parser.Result as Result
-- sync byteを含めた長さ
bytesLen :: BytesLen
bytesLen = 188

data ContinuityType = Continuous | Duplicated | ContinuousErr deriving (Eq,Show)

data Type =
  PES -- (Packetized Elementary Stream) https://en.wikipedia.org/wiki/Packetized_elementary_stream
  | PSI -- (Program Specific Information) https://en.wikipedia.org/wiki/Program-specific_information
  | Following -- following packet
  deriving (Eq,Show)

class (Header.Class t, Show t, Eq t) => Class t where
  -- please implement
  mkEOF :: t
  mkOK  :: Header.Data -> Maybe AdaptationField.Data -> ByteString -> t
  isEOF :: t -> Bool
  isOK  :: t -> Bool
  adaptation_field :: t -> Maybe AdaptationField.Data
  data_bytes :: t -> ByteString
  --

  -- 先頭パケットのdata_bytesの先頭3バイト (packet_start_code_prefix) が 0x000001 なら PES、そうでないなら PSI
  type_ :: t -> Type
  type_ x
    | Header.payload_unit_start_indicator x =
      case (BS.length $ data_bytes x) < 3 of
        True -> PSI
        False -> let bs = BS.unpack $ data_bytes x
                     b0 = bs !! 0
                     b1 = bs !! 1
                     b2 = bs !! 2
                 in if b0 == 0 && b1 == 0 && b2 == 1 then PES else PSI
    | otherwise = Following

  -- PSI の先頭パケットの data_byte は pointer_field から始まる
  payload :: t -> ByteString
  payload x = case type_ x of
                Following -> data_bytes x
                PSI -> let bs = data_bytes x in if BS.null bs then BS.empty else BS.tail bs
                PES -> data_bytes x

  (===) :: t -> t -> Bool
  (===) x y = x == y

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

  -- adaptation_field_length :: a -> Word64
  -- adaptation_field_length x = case adaptation_field x of
  --                               Nothing -> 0
  --                               Just af -> AdaptationField.adaptation_field_length af

  -- 引数のByteStringは同期用の先頭バイトは削られているものを想定している
  fromByteString :: ByteString -> Result.Data t
  fromByteString bytes =
    let plen'     = toInteger $ bytesLen - 1    :: Integer
        plen''    = fromInteger plen'           :: Int64
        byteslen' = toInteger $ BS.length bytes :: Integer
        bytes'    = BS.take plen'' bytes
        (header_bytes',body_bytes') = BS.splitAt 3 bytes' -- divide bytes to header and body
        header    = Header.parse $ BS.take 3 header_bytes'
        (res,rest) = AdaptationField.parse header body_bytes'
        data_bytes = if Header.has_payload header then rest else mkEmpty
    in
      if byteslen' < plen'
      then Result.DataIsTooShort $ Just $ fromInteger $ (plen' - byteslen')
      else case res of
        Result.Parsed maf       -> Result.Parsed $ mkOK header maf data_bytes
        Result.DataIsTooShort i -> Result.DataIsTooShort i
        Result.NotMatch         -> Result.NotMatch
        Result.SumCheckError    -> Result.SumCheckError
        Result.UnknownReason    -> Result.UnknownReason

  read :: (FH.Class fh) => fh -> IO (Result.Data t,(BS.ByteString,fh))
  read h = do
    h' <- FH.syncIO h
    isEOF' <- BytesReaderBase.isEOF h'
    if isEOF'
      then return (Result.Parsed mkEOF, (BS.empty,h'))
      else do
--      putStrLn $ show $ BytesReaderBase.loaded h'
      res@(bytes,h'') <- BytesReaderBase.getBytesIO h' (bytesLen - 1)
--      putStrLn "====="
--      putStrLn $ show $ BS.unpack $ bytes
      return (TS.Packet.fromByteString bytes,res)
      

data Data = MkData {
  _header  :: Header.Data,
  _maf     :: Maybe AdaptationField.Data,
  _data_bytes :: ByteString
  } | EOF | ContinuityCounterError deriving(Eq,Show)

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty (Just AdaptationField.mkEmpty) mkEmpty
  
instance Header.Class Data where
  header x@(MkData _ _ _) = _header x
  header  _               = mkEmpty :: Header.Data  
  
instance Class Data where
  adaptation_field x@(MkData _ _ _) = _maf x
  adaptation_field _                = Nothing

  data_bytes x@(MkData _ _ _) = _data_bytes x
  data_bytes _                = BS.empty
  
  isEOF EOF = True
  isEOF _   = False
  
  isOK (MkData _ _ _) = True
  isOK _              = False

  mkEOF = EOF
  mkOK h af b = MkData h af b
