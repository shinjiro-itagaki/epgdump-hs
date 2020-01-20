module TS.Reader where
-- import Data.ByteString.Lazy(hGet,length, unpack, take, pack, append,map, head, empty)
import qualified Data.ByteString.Lazy as BS
import Common(ByteString,PID)
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

import Data.Vector(Vector,snoc,null,empty)
import Data.Map(Map,lookup)
import qualified TS.FileHandle as FileHandle
import qualified TS.Packet as Packet
import Common(PIDs)
import qualified TS.Packet.Header as Header  
import qualified Data.Map.Lazy as Map
import qualified Parser.Result as Result
import SITables.Common(SITableIDs(..))

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

--eachTable :: (SITableIDs a) => [a] -> b -> (SITables.Data -> b -> IO (b,Bool)) -> IO b
--eachTable ids state hook = 

each :: String -> a -> (Packet.Data -> a -> FileHandle.ReadonlyData -> ByteString -> IO (Bool,a)) -> IO a
each path something act = do
  fh <- FileHandle.new path
  _each fh something act Map.empty

_each :: FileHandle.Data -> a -> (Packet.Data -> a -> FileHandle.ReadonlyData -> ByteString -> IO (Bool,a)) -> Map.Map PID Packet.Data -> IO a
_each fh something act countermap = do
  (resp,(bytes,fh')) <- Packet.read fh
  case resp of
    Result.Parsed p -> if Packet.isEOF p
      then return something
      else if Packet.isOK p then -- 
             case Packet.continuityChecked p (Map.lookup (Header.pid p) countermap) of
               Nothing -> _each fh' something act countermap -- 重複しているので何もせず捨てて処理を続行
               Just x -> 
                 act p something (FileHandle.getReadonlyInfo fh') bytes
                 >>= (\(continue,something') ->
                        if continue
                        then _each fh' something' act (Map.insert (Header.pid p) p countermap)
                        else return something'
                     )
           else
             _each fh' something act countermap
    _ -> _each fh' something act countermap -- パース失敗でスキップ

  
-- instance HolderIO.Class Data where
--   -- getBytesIO :: (Integral i) => a -> i -> IO (ByteString, a)
--   getBytesIO d i = _getBytesIO (_cur_sym d) d i
--   -- isEOF      :: a -> IO Bool
--   isEOF     _ = True
--   -- cache      :: a -> ByteString
--   cache     _ = BS.empty
--   -- clearCache :: a -> a
--   clearCache x = x
