{-# LANGUAGE FlexibleInstances #-}

module SITables where
import qualified SITables.Base as Base

import qualified SITables.BAT
import qualified SITables.BIT
import qualified SITables.Common
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

import Data.ByteString(ByteString)
import qualified TS.Packet as Packet
--import TS.Packet(FromPackets(..))
import Parser(ParseResult(..),mapParseResult)
import qualified BytesReader.HolderIO as HolderIO
import qualified SITables.Header1 as Header1
import Common(EmptyExist(..),PID,matchPID)
import qualified Parser.Result as Result
import Parser.Result((>>===))
import Data.Maybe(fromMaybe)

import SITables.Common(SITableIDs(..),(==.=))

type Callback d state = (d -> state -> IO state)

data Callbacks state = MkCallbacks {
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
  _cb_TOT  :: Maybe (Callback SITables.TOT.Data  state )
  }
  
matchPID :: PID -> Callbacks state -> Bool
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
    impl' :: (Base.Class d) => (d -> state -> IO state) -> Callbacks state -> Bool
    impl' callback callbacks2 =
      let empdata = mkEmpty
          _ = callback empdata -- empdata の型を推論できるようにするための記述で、実行はされない
      in (pids empdata) `Common.matchPID` pid
      

parseIO_simple :: (HolderIO.Class bh, Base.Class d) => bh -> Maybe d -> IO (Result.Data d,bh)
parseIO_simple bh init = Header1.parseIO bh >>=== (\(h,bh2) -> Base.parseIO (fromMaybe mkEmpty init) h bh2)

parseIO :: (HolderIO.Class bh) => bh -> state -> Callbacks state -> IO state
parseIO bh state callbacks = do
  (res_header1,bh2) <- Header1.parseIO bh
  case res_header1 of
    Result.Parsed x -> _parseIO bh2 x state callbacks  -- 終了
    _               -> return state -- ヘッダのパースエラーにつき続行不能で終了


_parseIO :: (HolderIO.Class bh) => bh -> Header1.Data -> state -> Callbacks state -> IO state
_parseIO bh header1 state callbacks =
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
    _                                -> return state
  where
    impl' f' callbacks2 = do
      (res,bh2) <- Base.parseIO mkEmpty header1 bh
      case res of
        Result.Parsed x -> f' x state -- 更新した状態を返して終了
        Result.NotMatch -> _parseIO bh2 header1 state callbacks2 -- マッチしなかったので次に進む
        _               -> return state -- エラー発生につき終了

instance EmptyExist (Callbacks a) where
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
    _cb_TOT  = Nothing
    }
