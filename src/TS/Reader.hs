module TS.Reader where
-- import Data.ByteString.Lazy(hGet,length, unpack, take, pack, append,map, head, empty)
import Data.Word(Word64, Word32, Word16, Word8)
import qualified Data.ByteString.Lazy as BS
import Common(ByteString,PID,EmptyExist(..),BytesLen)
import Data.ByteString.Lazy.Char8(pack)
import Data.Bits((.&.),(.|.),shiftL,shiftR,Bits)
import Data.Char(intToDigit,digitToInt)
import Data.Int(Int64)
import qualified TS.FileHandle as FileHandle
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import qualified Parser
import qualified BytesReader
import qualified BytesReader.HolderIO as HolderIO
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
import Data.Maybe(fromMaybe,isJust)
import qualified BytesReader.StockedBitsLen as StockedBitsLen
import qualified BytesReader.Status as Status
import Data.Foldable(toList)

class (BytesReader.Class a) => Class a where

type PacketCache = C.Seq Packet.Data

splitByFirstBlock :: PacketCache -> (PacketCache, PacketCache)
splitByFirstBlock cache =
  let synced' = C.dropWhileL (not . Header.payload_unit_start_indicator) cache -- 最初のスタート地点の前にあるパケットを削除 xxxHxxxxxHxxx... => HxxxxxHxxx...
      mfst'   = synced' C.!? 0 --  HxxxxxHxxx... => H
      rest'   = C.drop 1 $ synced' -- xxxxxHxxx...
      tail'   = C.takeWhileL (not . Header.payload_unit_start_indicator) $ rest' -- 先頭パケットの次から次のスタート地点までを取得 xxxxxHxxx... => xxxxx
      others' = C.dropWhileL (not . Header.payload_unit_start_indicator) $ rest' -- 次の先頭以降を取得  xxxxxHxxx... => Hxxx...
      oneset' = C.fromList $ Packet.continuityChecked $ toList $ case mfst' of
                  Nothing   -> C.empty
                  Just fst' -> fst' C.<| tail'
  in
    if C.null oneset' && isJust mfst' -- パケット欠損があった場合
    then splitByFirstBlock others'
    else (oneset', others')

data ByteStringHolder = MkByteStringHolder {
  _data           :: ByteString,
  _pos            :: Word64, -- bits
  _size           :: BytesLen,
  _loaded         :: Word8,
  _stockedBitsLen :: StockedBitsLen.Data,
  _bytesCounter   :: BytesLen,
  _cache          :: C.Seq ByteString
  }

toByteStringHolder :: ByteString -> ByteStringHolder
toByteStringHolder bs = MkByteStringHolder {
  _data           = bs,
  _pos            = 0,
  _size           = fromInteger $ toInteger $ BS.length bs,
  _loaded         = 0,
  _stockedBitsLen = StockedBitsLen.Zero,
  _bytesCounter   = 0,
  _cache          = C.empty
  }

instance Status.Class ByteStringHolder where
  pos = _pos
  size = _size

instance Counter.Class ByteStringHolder where
  getBytesCounter     = _bytesCounter 
  resetBytesCounter x = x {_bytesCounter = 0 }

instance HolderIO.Class ByteStringHolder where
  isEOF        = return . BS.null . _data
  getBytesIO   = BytesReader.getBytes
  getBitsIO    = BytesReader.getBits
  cache        = (foldl BS.append BS.empty) . _cache
  clearCache x = x { _cache = C.empty }

instance BytesReader.Class ByteStringHolder where
  pos            = _pos
  size           = _size
  loaded         = _loaded
  stockedBitsLen = _stockedBitsLen
  updateStockedBitsLen x bitslen = x {_stockedBitsLen = bitslen }
  getBytes x i
    | i < 1             = return (BS.empty, x)
    | BS.null $ _data x = return (BS.empty, x)
    | otherwise = 
      let i1 = (fromInteger $ toInteger i) :: Int64
          (bytes,rest) = BS.splitAt i1 $ _data x
          i2 = (fromInteger $ toInteger $ BS.length bytes) :: Word64
      in 
        return $ if BS.null bytes
        then (BS.empty, x)
        else (bytes, x {
                 _data           = rest,
                 _pos            = (_pos x) + i2,
                 _cache          = ((_cache x) C.|> bytes),
                 _loaded         = BS.last bytes,
                 _bytesCounter   = (_bytesCounter x) + i2,
                 _stockedBitsLen = StockedBitsLen.Zero
                 }
             )
             
_parseFromCache :: (Base.Class d) => PacketCache -> IO (Result.Data d,PacketCache)
_parseFromCache cache =
  let emptable = mkEmpty
      pids' = pids emptable
      (matched',others') = C.partition (\p -> pids' `Common.matchPID` (Header.pid p)) cache
      (block', rest') = splitByFirstBlock matched'
      payload' = toByteStringHolder $ foldl (\res p -> BS.append res (Packet.payload p)) BS.empty block'
  in do
    (res,_) <- SITables.parseIO_simple payload' (Just emptable)
    return (res,rest')
  
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
      (res,cache2) <- _parseFromCache cache
      case res of
        Result.NotMatch -> _fireCallback callbacks2 state (True,cache) -- テーブルがマッチしていない場合は次に移動
        Result.Parsed d -> do
          (continue, state2) <- callback d state
          if continue
            then _fireCallback callbacks2 state2 (True,cache2) -- パース成功した場合はコールバックを呼び出して他にテーブルが作れないか検索
            else return (cache2,state2) -- 続行フラグがfalseなので処理を終了
        _               -> return (cache2,state) -- 失敗したので終了
          
_appendPacketAndFireCallback :: PacketCache -> Callbacks state -> state -> Packet.Data -> IO (PacketCache,state)
_appendPacketAndFireCallback cache callbacks state packet =
  let pid = Header.pid packet
      indicator = Header.payload_unit_start_indicator packet -- ペイロードの開始地点のパケットかどうか
  in _fireCallback callbacks state $
     if SITables.matchPID pid callbacks --取得すべきpidかどうか
     then
       (indicator, cache C.|> packet) -- パケットを追加済みのキャッシュを返す。パケットの開始地点であれば、以前までに蓄積したパケットからテーブルを構築する
     else
       (False, cache) -- 取得すべきpidではないのでパケットを追加しない

eachTable :: String -> Callbacks state -> state -> Maybe (Packet.Data -> state2 -> FileHandle.ReadonlyData -> IO (Bool,state2), state2) -> IO state
eachTable path callbacks state mx = do
  fh <- FileHandle.new path
  _eachTable fh callbacks state mx

_eachTable :: FileHandle.Data -> Callbacks state -> state -> Maybe (Packet.Data -> state2 -> FileHandle.ReadonlyData -> IO (Bool,state2), state2) -> IO state
_eachTable fh callbacks state mx = do
  (cache,state',mx') <- _each fh (C.empty,state,mx) impl'
  return state'
  where
    -- Packet.Data -> state -> FileHandle.ReadonlyData -> IO (Bool,state)
    impl' packet' (cache',state',mx') fhd' _ = do
      (cache'',state'') <- _appendPacketAndFireCallback cache' callbacks state' packet'
      (mx'',continue'') <-
        case mx' of
          Just (f,state2) -> do (continue',state2') <- f packet' state2 fhd'
                                return (Just (f,state2'),continue')
          Nothing -> return (mx',True)
      return (continue'', (cache'',state'',mx'))

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
