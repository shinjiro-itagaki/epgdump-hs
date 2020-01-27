module TS.Reader where
-- import Data.ByteString.Lazy(hGet,length, unpack, take, pack, append,map, head, empty)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8(pack)
import Utils
import qualified TS.FileHandle as FileHandle
import qualified BytesReader
import qualified BytesReader.Base as BytesReaderBase
import qualified BytesReader.Counter as Counter

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

import qualified Data.Vector as V
import qualified TS.FileHandle as FileHandle
import qualified TS.Packet as Packet
import qualified TS.Packet.Header as Header
import qualified Data.Map.Lazy as MapL
import qualified Parser.Result as Result
-- import SITables(Callback,Callbacks(..),parseIO)
import SITables(Callback,Callbacks(..))
import qualified SITables.Header1 as Header1
import Data.Maybe(fromMaybe,isJust)
import qualified BytesReader.StockedBitsLen as StockedBitsLen
import qualified BytesReader.Status as Status
import Data.Foldable(toList)

class (BytesReader.Class a) => Class a where

type PacketCache = V.Vector Packet.Data

-- 引数のパケット一覧はpidがすべて同じであることを前提にしている
splitByFirstBlock :: PacketCache -> (PacketCache, PacketCache)
splitByFirstBlock cache =
  let synced' = V.dropWhile (not . Header.payload_unit_start_indicator) cache -- 最初のスタート地点の前にあるパケットを削除 xxxHyyyyyHzzz... => HxxxxxHyyy...
      mfst'   = synced' V.!? 0     --  HxxxxxHyyy... => H
      rest'   = V.drop 1 $ synced' --  HxxxxxHyyy... => xxxxxHyyy...

      -- tail'   : 先頭パケットの次から次のスタート地点まで (H)xxxxxHyyy... => (H)xxxxx
      -- others' : 次の先頭以降  (H)xxxxxHyyy... => Hyyy...
      (tail',others') = V.span (not . Header.payload_unit_start_indicator) $ rest'
      oneset' = V.fromList $ Packet.continuityChecked $ Utils.toList $ case mfst' of
                  Nothing   -> V.empty
                  Just fst' -> fst' `V.cons` tail'
      packet_losted = V.null oneset' && isJust mfst' -- 先頭パケットがあるのに連続チェック済みのパケット一覧が空 => パケット欠損があったことを示す
  in
    if packet_losted
    then splitByFirstBlock others' -- パケット欠損がある場合は残りを対象にして処理を続行
    else (oneset', others') -- パケット欠損がないのでそのまま返す

_fireCallback :: (Show state) =>  Callbacks state -> state -> PID -> PacketCache -> IO (Result.Data Bool, PacketCache, state)
_fireCallback callbacks state pid cache =
  let (pid_matched',pid_unmatched') = V.partition (\p -> pid == (Header.pid p)) cache
      (block', rest') = splitByFirstBlock pid_matched'
      bytes = foldl (\res p -> BS.append res (Packet.payload p)) BS.empty block'
      restall = rest' V.++ pid_unmatched'
  in do
    putStrLn "-------"
    putStrLn $ ("pid=" ++) $ show $ pid
--    putStrLn $ show $ BS.unpack $ bytes
    (res,state2) <- SITables.parse state callbacks bytes
    case res of
      Result.Parsed b -> if b
                         then _fireCallback callbacks state2 pid restall -- 次も実行してみる
                         else return (res,restall,state2) -- 結果をそのまま返す
      x               -> return (Result.map (\_ -> True) x,restall,state2)

-- コールバックが存在するテーブルのpidの場合のみパケットをキャッシュに追加し、必要があれば消費する
_appendPacketAndFireCallback :: (Show state) => PacketCache -> Callbacks state -> state -> Packet.Data -> IO (Maybe (Result.Data Bool), PacketCache,state)
_appendPacketAndFireCallback cache callbacks state packet =
  let pid = Header.pid packet
      indicator = Header.payload_unit_start_indicator packet -- ペイロードの開始地点のパケットかどうか
      pid_matched = SITables.matchPID pid callbacks
      toNextCache = (\x@(mresb,cache',state') -> return (mresb, if pid_matched then cache' `V.snoc` packet else cache', state'))
  in do
    if pid_matched then
      toNextCache =<<
      if indicator then
        (\(resb,cache,state) -> return (Just resb,cache,state)) =<< _fireCallback callbacks state pid cache
      else
        return (Nothing, cache,state) -- まだ続きがあるかもしれないので何もしない
    else return (Nothing, cache,state) -- 取得すべきpidではないのでパケットを追加しない


eachTable :: (Show state,Show state2) => String -> Callbacks state -> state -> Maybe (Packet.Data -> state2 -> FileHandle.ReadonlyData -> IO (Bool,state2), state2) -> IO state
eachTable path callbacks state mx = do
  fh <- FileHandle.new path
  _eachTable fh callbacks state mx

_eachTable :: (Show state, Show state2) => FileHandle.Data -> Callbacks state -> state -> Maybe (Packet.Data -> state2 -> FileHandle.ReadonlyData -> IO (Bool,state2), state2) -> IO state
_eachTable fh callbacks state mf = do
  (cache,state',mf') <- _eachPacket fh (V.empty,state,mf) impl'
  return state'
  where
    impl' packet' (cache',state',mf') fhd' _ = do
      (mres, cache'',state'') <- _appendPacketAndFireCallback cache' callbacks state' packet'
      let next = case mres of
                   Just (Result.Parsed False) -> False
                   _                          -> True
        in do
        (mf'',continue'') <-
          case mf' of -- eachPacketのフックが設定されている場合
            Just (f,state2) -> do (continue',state2') <- f packet' state2 fhd'
                                  return (Just (f,state2'),next && continue')
            Nothing -> return (mf',next && True)
        return (continue'', (cache'',state'',mf'))

eachPacket :: String -> a -> (Packet.Data -> a -> FileHandle.ReadonlyData -> ByteString -> IO (Bool,a)) -> IO a
eachPacket path something act = do
  fh <- FileHandle.new path
  _eachPacket fh something act

_eachPacket :: FileHandle.Data -> a -> (Packet.Data -> a -> FileHandle.ReadonlyData -> ByteString -> IO (Bool,a)) -> IO a
_eachPacket fh something act = do
  (resp,(bytes,fh')) <- Packet.read fh
  case resp of
    Result.Parsed p -> if Packet.isEOF p
      then return something -- EOFにつき終了
      else if Packet.isOK p
           then do
             act p something (Status.getStatus fh') bytes
             >>= (\(continue,something') ->
                     if continue
                     then _eachPacket fh' something' act
                     else return something' -- 続行しないという指定があったので終了
                 )
           else
             _eachPacket fh' something act
    _ -> _eachPacket fh' something act -- パース失敗でスキップ
