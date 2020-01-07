{-# LANGUAGE FlexibleInstances #-}

module TS
    ( FileHandle(tspackets,syncByte) -- MkFileHandle と handleは非公開
      ,mkFileHandle
      ,syncIO
      ,isEOF
      ,isOK
      ,TSpacket
      ,TSpacketTypeable(..)
      ,eachTSpackets
    ) where

import System.IO(
  Handle
  ,hIsEOF
  ,SeekMode(..)
  ,hSeek
  ,hFileSize
  )
  
import Data.ByteString(hGet,hGetSome,length, ByteString,unpack, take, pack, append,map, head)
import Data.Vector(Vector,empty,snoc,length)
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Char8(pack)
import Data.Bits((.&.),(.|.),shiftL,shiftR,Bits)
import Data.Char(intToDigit,digitToInt)

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

class TSpacketTypeable t where
  typeNum :: t -> Word16
  
  -- NIT（Network Information Table）
  -- チャンネル番号や変調方式、ガードインターバルなど、送信するネットワークに関する情報が含まれる。NITのPIDは必ず0x10と決まっている。
  isNIT :: t -> Bool

  -- SDT（Service Description Table）
  -- チャンネルの名称が含まれる。また各チャンネル（サービス）で送出されるEITの種類、デジタルコピー制御情報も含まれる。SDTのPIDは必ず0x11と決まっている。
  isSDT :: t -> Bool

  -- EIT（Event Information Table）
  -- 番組の名称や放送日時、放送内容など番組に関連する情報が含まれる。EPGは主にこの情報を用いて作成される。EITのPIDは必ず0x12と決まっている。
  -- EITはその示す内容により自局の現在及び次の番組、他局の現在及び次の番組、自局のそれ以外を含む番組、他局のそれら以外を含む番組の4種類に大別される。自局の現在及び次の番組に関する情報は必ず送出する必要があるがその他の情報は送出が任意であり、また送出の頻度も重要性に応じて小さくすることができる。
  isEIT :: t -> Bool
  
  isRST :: t -> Bool

  -- TDT（Time Date Table）
  -- 現在の日付、時刻に関する情報が含まれる。ただしTOTにはTDTの全ての情報が含まれるため送信されるのはTDTかTOTのいずれかのみ（PIDは双方とも0x14）と規定されており、実際のところTDTだけが送信されている。
  isTDT :: t -> Bool

  -- SDTT（Software Download Trigger Table）
  -- デジタル放送受信機の最新ソフトウェア（Engineering Stream Serviceで伝送される）の伝送スケジュールが記載される。本テーブルは、全放送局で全く同一の情報が伝送されている。実際のデータはARIB STD-B16で規定されるDCT（Download Control Table）、DLT（Download Table）で伝送される。
  isSDTT:: t -> Bool

  -- BIT（Broadcaster Information Table）
  -- 放送局識別情報（BSD）や系列情報（地上D）、各SI（Service Information）テーブルの再送周期、SIの掲載期間、ロゴ情報の伝送方法など放送局のSI送信情報が含まれる。BITのPIDは必ず0x24と決まっている。
  isBIT :: t -> Bool
 
  isNIT = (0x10==) . typeNum
  isSDT = (0x11==) . typeNum
  isEIT = (0x12==) . typeNum
  isRST = (0x13==) . typeNum
  isTDT = (0x14==) . typeNum
  isSDTT= (\x -> x == 0x23 || x == 0x28) . typeNum
  isBIT = (0x24==) . typeNum

-- adaptation_field_control
-- 上1ビットが立っている(2 or 3)場合はadaptation_fieldがある
-- 下1ビットが立っている(1 or 3)場合はpayloadがある（ヘッダに続いて直ちにpayloadが始まる）
-- 32bit
class TSpacketTypeable h => TSpacketHeader h where
--  sync_byte                    :: h -> Word8 -- 8 bits
  transport_error_indicator    :: h -> Bool -- 1 bits
  payload_unit_start_indicator :: h -> Bool -- 1 bits
  transport_priority           :: h -> Bool -- 1 bits
  pid                          :: h -> Word16 -- 13 bits
  transport_scrambling_control :: h -> Word8 -- 2 bits
  adaptation_field_control     :: h -> Word8 -- 2 bits

  -- トランスポートストリームのエラー検出
  -- トランスポートパケットヘッダには同じPID内において値が1ずつ増加するcontinuity_counterという4ビットのカウンタがあり、TS受信側でこのカウンタの不連続を検出することでトランスポートパケットのロストを検出することができる。
  continuity_counter           :: h -> Word8 -- 4 bits
  fromByteString               :: ByteString -> h

  

data FileHandle = MkFileHandle {
  handle :: Handle,
  tspackets :: Vector TSpacket,
  pos :: Word64, -- bits
  syncByte :: Word8
  }

filesize :: FileHandle -> IO Word64
filesize = (return . fromInteger =<<) . hFileSize . handle

getBytes :: FileHandle -> Int -> IO (ByteString, FileHandle)
getBytes fh i = do
  bytes <- Data.ByteString.hGetSome (handle fh) i
  return (bytes, fh { pos = (pos fh) + i' })
  where
    i' = fromInteger $ toInteger (i * 8)

seekBytes :: FileHandle -> Integer -> IO FileHandle
seekBytes fh i = do
  hSeek (handle fh) RelativeSeek i >> return fh { pos = (pos fh) + i' }
  where
    i' = fromInteger i
    
syncByteValue :: Word8
syncByteValue = 0x47

mkFileHandle :: Handle -> FileHandle
mkFileHandle fh = MkFileHandle {
  tspackets = Data.Vector.empty,
  pos = 0,
  handle = fh,
  syncByte = syncByteValue
}

syncIO :: FileHandle -> IO FileHandle
syncIO fh = do
  isEOF <- hIsEOF $ handle fh
  if isEOF
    then return fh
    else
    do
      (bytes,fh2) <- getBytes fh 1
      x <- return $ Data.ByteString.head bytes
      if x == syncByte fh2
        then return fh2
        else syncIO fh2

readTSpacket :: FileHandle -> (Word32 -> Bool) -> IO (TSpacket, FileHandle, ByteString)
readTSpacket orgfh selector = do
  fh <- syncIO orgfh
  (bytes,fh2) <- getBytes fh lenTSpacket
  let
    p = mkTSpacketFromByteString bytes
    fh3 = if selector $ header p
      then fh2 { tspackets = snoc (tspackets fh2) p }
      else fh2
  -- putStr "========\n"  
  -- putStr $ show $ header p
  -- putStr "\n"
  -- putStr $ show $ pid $ header p
  -- putStr "\n======="  
  return (p, fh3, bytes)

mkTSpacketFromByteString :: ByteString -> TSpacket
mkTSpacketFromByteString bytes =
  if Data.ByteString.length bytes < lenTSpacket - 1
  then MkEofTSpacket
  else MkTSpacket {
    header = fromByteString $ Data.ByteString.take 4 bytes,
    body = MkEmptyTSpacketBody
    }

readNum :: (Integral a, Bits a, Num b) => a -> a -> b
readNum mask x = fromInteger $ toInteger $ (x .&. mask) `shiftR` (sftcount mask 0)
  where
    sftcount 0 count = count
    sftcount mask' count = if (mask' .&. 1) == 1 then count else sftcount (mask' `shiftR` 1) (count + 1)

_TSPAYLOADMAX :: Int
_TSPAYLOADMAX=184

lenTSpacket :: Int
lenTSpacket = 188 -- bytes

instance TSpacketHeader Word32 where
--  sync_byte                    = readNum 0x000000ff -- 0b00000000000000000000000011111111
  transport_error_indicator    = (1 ==) . (.&. (1 :: Word8)) . (readNum 0x00000100) -- 0b00000000000000000000000100000000
  payload_unit_start_indicator = (1 ==) . (.&. (1 :: Word8)) . (readNum 0x00000200) -- 0b00000000000000000000001000000000
  transport_priority           = (1 ==) . (.&. (1 :: Word8)) . (readNum 0x00000400) -- 0b00000000000000000000010000000000
  pid                          = readNum 0x00fff800 -- 0b00000000111111111111100000000000
  transport_scrambling_control = readNum 0x00300000 -- 0b00000011000000000000000000000000
  adaptation_field_control     = readNum 0x0c000000 -- 0b00001100000000000000000000000000
  continuity_counter           = readNum 0xf0000000 -- 0b11110000000000000000000000000000
  fromByteString             x = let n = 3 :: Int
                                     wordsmap = zip [0 .. n] $ unpack x
                                 in foldl (.|.) 0 $ Prelude.map (\idx -> (fromInteger $ toInteger $ find' wordsmap 0 idx) `shiftL` (8*(n - idx))) [0 .. n]
    where
      find' :: Eq a => [(a, b)] -> b -> a -> b
      find' arr df idx =
        case lookup idx arr of
          Nothing -> df
          Just x  -> x

instance TSpacketTypeable Word32 where
  typeNum = pid
  
data TSpacketBody = MkEmptyTSpacketBody | MkTSpacketBody {
  adaptation_field :: Maybe Int,
  payload          :: [Char],
  payloadlen       :: Int,
  rcount           :: Int  
  }


data RawTSpacket h = MkEmptyTSpacket | MkEofTSpacket | MkTSpacket {
  header :: h,
  body :: TSpacketBody
}

type TSpacket = RawTSpacket Word32 

isEOF :: TSpacket -> Bool
isOK :: TSpacket -> Bool

isEOF MkEofTSpacket = True
isEOF _ = False
isOK (MkTSpacket _ _) = True
isOK _ = False


eachTSpackets :: FileHandle -> a -> (Word32 -> Bool) -> (TSpacket -> FileHandle -> a -> ByteString -> IO (a, Bool)) -> IO (FileHandle,a)
eachTSpackets fh something selector f = do
  -- putStr "hoge"
  -- return (fh,something)
  impl' fh something
  where
    impl' fh' something' = do
      (p,fh'',bytes') <- readTSpacket fh' selector
      if isEOF p
        then return (fh'',something')
        else f p fh'' something' bytes' >>= (\(something'', continue) -> if continue then impl' fh'' something'' else return (fh'', something''))

instance TSpacketTypeable TSpacket where
  -- MkEmptyTSpacket | MkEofTSpacket | MkTSpacket
  typeNum x = case x of
    MkTSpacket header' body' -> typeNum header'
    _          -> 0x00


