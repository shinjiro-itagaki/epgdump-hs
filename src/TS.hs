{-# LANGUAGE FlexibleInstances #-}

module TS (
  each
  ) where

import Data.ByteString.Lazy(hGet,length, ByteString,unpack, take, pack, append,map, head)
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Lazy.Char8(pack)
import Data.Bits((.&.),(.|.),shiftL,shiftR,Bits)
import Data.Char(intToDigit,digitToInt)
import Data.Int(Int64)
import qualified TS.FileHandle as FileHandle
import Parser(ParseResult(..),parseFlow,(|>>=),flowStart,getBitsIO_M,mapParseResult,parseIO,ParseIOFlow,execParseIOFlow)
import qualified Parser

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

{-
番組より小さい単位（番組内イベント）の内容を記述するための、さらにいくつかの拡張が行われている。

LIT（Local Event Information Table）
    番組内イベントに関する情報が含まれる。
ERT（Event Relation Table）
    番組、および番組内イベントの関係に関する情報が含まれる。
ITT（Index Transmission Table）
    番組内イベントに関係する時刻情報が含まれる。時刻情報はLITにも含まれているが放送ごとに異なる値が必要になるため、LITとは別に送出される。

ARIB STD-B1、B21では以下のような情報も規定されている。

SDTT（Software Download Trigger Table）
    デジタル放送受信機の最新ソフトウェア（Engineering Stream Serviceで伝送される）の伝送スケジュールが記載される。本テーブルは、全放送局で全く同一の情報が伝送されている。実際のデータはARIB STD-B16で規定されるDCT（Download Control Table）、DLT（Download Table）で伝送される。
CDT（Common Data Table）（地上D）
    EPG画面等で表示される、各サービス毎に対応したロゴ情報が伝送される。ロゴ情報は1サービスあたり6種類（それぞれ大きさが異なるpngファイル）がセクション化され、伝送される。
-}

import qualified TS.Packet as Packet
import qualified Data.Map.Lazy as Map
import Common(PID)
import qualified TS.Packet.Header as Header
import qualified Parser.Result as Result

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
