{-# LANGUAGE FlexibleInstances #-}

module SITables where
import qualified Utils.Matcher as Matcher
import qualified SITables.Base as Base
import qualified SITables.BAT
import qualified SITables.BIT
import qualified SITables.EIT
import qualified SITables.LDT
import qualified SITables.NBIT
import qualified SITables.NIT
import qualified SITables.PCAT
import qualified SITables.RST
import qualified SITables.SDT
import qualified SITables.ST
import qualified SITables.TDT
import qualified SITables.TOT
import qualified Utils.EmptyExist as EmptyExist
import qualified TS.Packet as Packet
import qualified BytesReader.Base as BytesReaderBase
import qualified BytesReader
import qualified SITables.Header1 as Header1
import qualified Parser.Result as Result
import Parser.Result((>>===))
import Data.Maybe(fromMaybe)
import qualified Utils.FromByteString as FromByteString
import qualified Data.ByteString.Lazy as BS
import Utils
--import Utils.FromByteString(fromByteStringWithRest)
type Callback d state = (d -> state -> IO (Bool,state))

data (Show state) => Callbacks state = MkCallbacks {
  _cb_BAT  :: Maybe (Callback SITables.BAT.Data  state ),
  _cb_BIT  :: Maybe (Callback SITables.BIT.Data  state ),
  _cb_EIT  :: Maybe (Callback SITables.EIT.Data  state ),
  _cb_LDT  :: Maybe (Callback SITables.LDT.Data  state ),
  _cb_NBIT :: Maybe (Callback SITables.NBIT.Data state ),
  _cb_NIT  :: Maybe (Callback SITables.NIT.Data  state ),
  _cb_PCAT :: Maybe (Callback SITables.PCAT.Data state ),
  _cb_RST  :: Maybe (Callback SITables.RST.Data  state ),
  _cb_SDT  :: Maybe (Callback SITables.SDT.Data  state ),
  _cb_ST   :: Maybe (Callback SITables.ST.Data   state ),
  _cb_TDT  :: Maybe (Callback SITables.TDT.Data  state ),
  _cb_TOT  :: Maybe (Callback SITables.TOT.Data  state ),
  _cb_ERR  :: Maybe (Callback (Result.Data Header1.Data) state ), -- ヘッダ の解析でエラーがあった場合
  _cb_UNM  :: Maybe (Callback Header1.Data state ) -- ヘッダの解析はできたが、どれにもマッチしなかった場合
  }--  deriving (Show)

-- 指定したPIDがコールバック中に含まれるか判定
matchPID :: (Show state) => PID -> Callbacks state -> Bool
matchPID pid callbacks =
  case callbacks of
    (MkCallbacks {_cb_BAT = Just f}) -> impl' f $ callbacks{_cb_BAT = Nothing}
    (MkCallbacks {_cb_BIT = Just f}) -> impl' f $ callbacks{_cb_BIT = Nothing}
    (MkCallbacks {_cb_EIT = Just f}) -> impl' f $ callbacks{_cb_EIT = Nothing}
    (MkCallbacks {_cb_LDT = Just f}) -> impl' f $ callbacks{_cb_LDT = Nothing}
    (MkCallbacks {_cb_NBIT= Just f}) -> impl' f $ callbacks{_cb_NBIT= Nothing}
    (MkCallbacks {_cb_NIT = Just f}) -> impl' f $ callbacks{_cb_NIT = Nothing}
    (MkCallbacks {_cb_PCAT= Just f}) -> impl' f $ callbacks{_cb_PCAT= Nothing}
    (MkCallbacks {_cb_RST = Just f}) -> impl' f $ callbacks{_cb_RST = Nothing}
    (MkCallbacks {_cb_SDT = Just f}) -> impl' f $ callbacks{_cb_SDT = Nothing}
    (MkCallbacks {_cb_ST  = Just f}) -> impl' f $ callbacks{_cb_ST  = Nothing}
    (MkCallbacks {_cb_TDT = Just f}) -> impl' f $ callbacks{_cb_TDT = Nothing}
    (MkCallbacks {_cb_TOT = Just f}) -> impl' f $ callbacks{_cb_TOT = Nothing}
    _                                -> False
  where
    impl' :: (Base.Class d, Show state) => (d -> state -> IO (Bool,state)) -> Callbacks state -> Bool
    impl' callback callbacks2 =
      let empdata = mkEmpty
          _ = callback empdata -- empdata の型を推論できるようにするための記述で、実行はされない
      in if (pids empdata) `Matcher.matchPID` pid
         then True
         else SITables.matchPID pid callbacks2 -- マッチしなかった場合は次の候補を検索
      

parse :: (Show state) => state -> Callbacks state -> ByteString -> IO (Result.Data Bool,state)
parse st callbacks bs =
  let (header1,bs0) = fromByteStringWithRest bs
  in do
    putStrLn $ show $ BS.unpack $ bs
    parseAfterHeader1 header1 st callbacks bs0

parseAfterHeader1 :: (Show state) => Header1.Data -> state -> Callbacks state -> ByteString -> IO (Result.Data Bool,state)
parseAfterHeader1 header1 state callbacks bs = do
  putStrLn $ show header1
  case callbacks of
    (MkCallbacks {_cb_BAT = Just f}) -> impl' f $ callbacks{_cb_BAT = Nothing}
    (MkCallbacks {_cb_BIT = Just f}) -> impl' f $ callbacks{_cb_BIT = Nothing}
    (MkCallbacks {_cb_EIT = Just f}) -> impl' f $ callbacks{_cb_EIT = Nothing}
    (MkCallbacks {_cb_LDT = Just f}) -> impl' f $ callbacks{_cb_LDT = Nothing}
    (MkCallbacks {_cb_NBIT= Just f}) -> impl' f $ callbacks{_cb_NBIT= Nothing}
    (MkCallbacks {_cb_NIT = Just f}) -> impl' f $ callbacks{_cb_NIT = Nothing}
    (MkCallbacks {_cb_PCAT= Just f}) -> impl' f $ callbacks{_cb_PCAT= Nothing}
    (MkCallbacks {_cb_RST = Just f}) -> impl' f $ callbacks{_cb_RST = Nothing}
    (MkCallbacks {_cb_SDT = Just f}) -> impl' f $ callbacks{_cb_SDT = Nothing}
    (MkCallbacks {_cb_ST  = Just f}) -> impl' f $ callbacks{_cb_ST  = Nothing}
    (MkCallbacks {_cb_TDT = Just f}) -> impl' f $ callbacks{_cb_TDT = Nothing}
    (MkCallbacks {_cb_TOT = Just f}) -> impl' f $ callbacks{_cb_TOT = Nothing}
    (MkCallbacks {_cb_UNM = Just f}) -> f header1 state >>= return . (\(b,st) -> (Result.NotMatch,st)) -- どれにもマッチしない場合のコールバックが設定されている場合
    _                                -> return (Result.NotMatch,state)
  where
    impl' f' callbacks2 =
      let sample  = mkEmpty
          _       = f' sample -- 型を推論できるようにするための記述で、実行はされない
          matched = (table_ids sample) ==|= (Header1.table_id header1)
          res = if matched then Base.parseAfterHeader1 header1 bs else Result.NotMatch
      in case res of
           Result.Parsed x -> f' x state >>= return . (\(b,st) -> (Result.Parsed b,st)) -- 更新した状態を返して検索終了
           Result.NotMatch -> parseAfterHeader1 header1 state callbacks2 bs -- マッチしなかったので次の候補に進む
           x               -> return (Result.map (\_->True) x,state) -- エラー発生につきコールバックの検索を終了

instance (Show a) => EmptyExist.Class (Callbacks a) where
  mkEmpty = MkCallbacks {
    _cb_BAT  = Nothing,
    _cb_BIT  = Nothing,
    _cb_EIT  = Nothing,
    _cb_LDT  = Nothing,
    _cb_NBIT = Nothing,
    _cb_NIT  = Nothing,
    _cb_PCAT = Nothing,
    _cb_RST  = Nothing,
    _cb_SDT  = Nothing,
    _cb_ST   = Nothing,
    _cb_TDT  = Nothing,
    _cb_TOT  = Nothing,
    _cb_ERR  = Nothing,
    _cb_UNM  = Nothing
    }
