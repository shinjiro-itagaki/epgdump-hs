module TS.Reader where
-- import Data.ByteString.Lazy(hGet,length, unpack, take, pack, append,map, head, empty)
import qualified Data.ByteString.Lazy as BS
import Common(ByteString,PID,EmptyExist(..))
import Data.ByteString.Lazy.Char8(pack)
import Data.Bits((.&.),(.|.),shiftL,shiftR,Bits)
import Data.Char(intToDigit,digitToInt)
import Data.Int(Int64)
import qualified TS.FileHandle as FileHandle
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import qualified Parser
import qualified BytesReader
import qualified BytesReader.HolderIO as HolderIO

import qualified SITables
import qualified SITables.Base as Base
import qualified SITables.BAT as BAT
import qualified SITables.BIT as BIT
import qualified SITables.EIT as EIT
import qualified SITables.LDT as LDT
import qualified SITables.NBIT as NBIT
import qualified SITables.NIT as NIT
import qualified SITables.PCAT as PCAT
import qualified SITables.RST as RST
import qualified SITables.SDT as SDT
import qualified SITables.ST as ST
import qualified SITables.TDT as TDT
import qualified SITables.TOT as TOT

--import Data.Vector(Vector,snoc,null,empty)
--import qualified Data.Vector as V
import qualified Data.Sequence as C
import qualified TS.FileHandle as FileHandle
import qualified TS.Packet as Packet
import Common(PIDs,matchPID)
import qualified TS.Packet.Header as Header
import qualified Data.Map.Lazy as MapL
import qualified Parser.Result as Result
import SITables(Callback,Callbacks(..),parseIO)
import SITables.Common(SITableIDs(..))
import Data.Maybe(fromMaybe)

class (BytesReader.Class a) => Class a where

-- _each :: FileHandle.Data -> a -> (Packet.Data -> a -> FileHandle.ReadonlyData -> ByteString -> IO (Bool,a)) -> IO a
-- _readTS_And_Caching :: SymSITable -> Data -> IO Data
-- _readTS_And_Caching sym d =
--   let stocker = _stocker d
--   in do
--     stocker2 <- (_each (_handle d) stocker impl')
--   where
--     impl' packet' stocker' _ _ =
      
-- _getBytesIO :: (Integral i) => SymSITable -> Data -> i -> IO (ByteString, Data)
-- _getBytesIO sym d i =
--   let i' = toInteger i
--       stock = fromMaybe Data.ByteString.empty $ Map.lookup sym $ _stocker d
--       (stocked, rest) = BS.splitAt i stock
--   in if i' < (toInteger BS.length stock)
--      then 

-- Header.payload_unit_start_indicator

-- new :: FileHandle.Data -> [SymSITable] -> Data
-- new fh syms =
--   MkData {
--   _handle  = fh,
--   _stocker = Map.empty,
--   _syms    = syms,
--   _cur_sym = SymNone
--   }

-- hoge
--

type PacketCache = C.Seq Packet.Data


splitByFirstBlock :: PacketCache -> (PacketCache, PacketCache)
splitByFirstBlock cache =
  let synced' = C.dropWhileL (not . Header.payload_unit_start_indicator) cache -- 最初のスタート地点の前にあるパケットを削除 xxxHxxxxxHxxx... => HxxxxxHxxx...
      mfst'   = synced' C.!? 0 --  HxxxxxHxxx... => H
      rest'   = C.drop 1 $ synced' -- xxxxxHxxx...
      tail'   = C.takeWhileL (not . Header.payload_unit_start_indicator) $ rest' -- 先頭パケットの次から次のスタート地点までを取得 xxxxxHxxx... => xxxxx
      others' = C.dropWhileL (not . Header.payload_unit_start_indicator) $ rest' -- 次の先頭以降を取得  xxxxxHxxx... => Hxxx...
      oneset' = case mfst' of
                  Nothing   -> C.empty
                  Just fst' -> fst' C.<| tail'
  in
    -- 連番チェックが必要 -- hoge
    (oneset',others')
  
_parseFromCache :: (Base.Class d) => PacketCache -> IO (Maybe (d,PacketCache))
_parseFromCache cache =
  let emptable = mkEmpty
      pids' = pids emptable
      (matched',others') = C.partition (\p -> pids' `Common.matchPID` (Header.pid p)) cache
      (block', rest') = splitByFirstBlock matched'
  in do
--    parseres <- parseIO
    return $ if C.null block' then
      Just (emptable,cache) -- hoge
    else
      Just (emptable,cache)
  
_fireCallback ::  Callbacks state -> state -> (Bool, PacketCache) -> IO (PacketCache, state)
_fireCallback callbacks state (False,cache) = return (cache,state) -- fireを実施しない
_fireCallback callbacks state (True, cache) = --  return (cache,state)
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
    _                                -> return (cache, state)
  where
--    impl' :: (Base.Class d) => (d -> state -> IO state) -> Callbacks state -> IO (Vector PacketCache,state)
    impl' callback callbacks2 = do
      res <- _parseFromCache cache
      case res of
        Nothing         -> _fireCallback callbacks2 state (True,cache) -- パースを試みたがテーブルが得られなかった場合は次の候補に移動
        Just (d,cache2) -> do  -- テーブルを作成できたのでコールバックを呼び出し
          state2 <- callback d state
          -- _fireCallback callbacks2 state2 (True,cache2) -- 次に移動
          return (cache2,state2) -- 終了（他にテーブルを作成できる場合もあるけど、いったんここで終了）
          
_appendPacketAndFireCallback :: (EmptyExist state) => PacketCache -> Callbacks state -> state -> Packet.Data -> IO (PacketCache,state)
_appendPacketAndFireCallback cache callbacks state packet =
  let pid = Header.pid packet
      indicator = Header.payload_unit_start_indicator packet -- ペイロードの開始地点のパケットかどうか
  in _fireCallback callbacks state $
     if SITables.matchPID pid callbacks --取得すべきpidかどうか
     then
       (indicator, cache C.|> packet) -- パケットを追加済みのキャッシュを返す。パケットの開始地点であれば、以前までに蓄積したパケットからテーブルを構築する
     else
       (False, cache) -- 取得すべきpidではないのでパケットを追加しない
  
eachTable :: (EmptyExist state) => FileHandle.Data -> Callbacks state -> state -> IO state
eachTable fh callbacks state = do
  (cache,state') <- _each fh (C.empty,state) impl' --  ::
  return state'
  where
    -- Packet.Data -> cache -> FileHandle.ReadonlyData -> ByteString -> IO (Bool,cache)
    impl' packet' (cache',state') _ _ = do
      cache_and_state <- _appendPacketAndFireCallback cache' callbacks state' packet'
      return (True, cache_and_state)

each :: String -> a -> (Packet.Data -> a -> FileHandle.ReadonlyData -> ByteString -> IO (Bool,a)) -> IO a
each path something act = do
  fh <- FileHandle.new path
  _each fh something act

_each :: FileHandle.Data -> a -> (Packet.Data -> a -> FileHandle.ReadonlyData -> ByteString -> IO (Bool,a)) -> IO a
_each fh something act = do
  (resp,(bytes,fh')) <- Packet.read fh
  case resp of
    Result.Parsed p -> if Packet.isEOF p
      then return something -- EOFにつき終了
      else if Packet.isOK p
           then --
             act p something (FileHandle.getReadonlyInfo fh') bytes
             >>= (\(continue,something') ->
                     if continue
                     then _each fh' something' act
                     else return something' -- 続行しないという指定があったので終了
                 )
           else
             _each fh' something act
    _ -> _each fh' something act -- パース失敗でスキップ

  
-- instance HolderIO.Class Data where
--   -- getBytesIO :: (Integral i) => a -> i -> IO (ByteString, a)
--   getBytesIO d i = _getBytesIO (_cur_sym d) d i
--   -- isEOF      :: a -> IO Bool
--   isEOF     _ = True
--   -- cache      :: a -> ByteString
--   cache     _ = BS.empty
--   -- clearCache :: a -> a
--   clearCache x = x
