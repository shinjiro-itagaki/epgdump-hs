{-# LANGUAGE FlexibleInstances #-}
module TS.FileHandle where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Lazy(hGet,ByteString,head,last)
import qualified Data.ByteString.Lazy as BS
import System.IO(Handle,hIsEOF,SeekMode(..),hSeek,hFileSize,openFile, IOMode(ReadMode))
import Data.Vector(Vector,empty,toList)
import Common(BytesLen,BytesHolderIO(..),BytesCounter(..))
import Data.Ratio(Ratio)
import Data.Bits(shiftL,shiftR,(.|.),(.&.))

class ReadonlyInfo a where
  pos :: a -> BytesLen
  filesize :: a -> BytesLen
  progress :: a -> Rational
  progress x = (toRational $ pos x) / (toRational $ filesize x)

  progress_percent :: (Fractional b) => a -> b
  progress_percent = fromRational . (* 100) . progress

data ReadonlyData = MkReadonlyData {
  __pos :: Word64,
  __filesize :: BytesLen
  }

instance ReadonlyInfo ReadonlyData where
  pos = __pos
  filesize = __filesize

-- ロード済みの在庫Word8のうち何ビットが残っているかを示すフラグ
-- 在庫の消費は左側ビットが優先
data StockedBitsLen = Zero | One | Two | Three | Four | Five | Six | Seven | All deriving (Enum,Bounded,Eq,Ord)

_stockedValueMask :: (Num a) => StockedBitsLen -> a
_stockedValueMask x =
  case x of
    Zero  -> 0x00
    One   -> 0x01
    Two   -> 0x03
    Three -> 0x07
    Four  -> 0x0F
    Five  -> 0x1F
    Six   -> 0x3F
    Seven -> 0x7F
    All   -> 0xFF

_stockedBitsNumLen :: (Num a) => StockedBitsLen -> a
_stockedBitsNumLen x =
  case x of
    Zero  -> 0
    One   -> 1
    Two   -> 2
    Three -> 3
    Four  -> 4
    Five  -> 5
    Six   -> 6
    Seven -> 7
    All   -> 8

_numToStockedBitsLen :: (Integral a) => a -> StockedBitsLen
_numToStockedBitsLen i
  | i < 0 = Zero
  | i > 8 = All
  | otherwise =
    case i of
      0 -> Zero
      1 -> One
      2 -> Two
      3 -> Three
      4 -> Four
      5 -> Five
      6 -> Six
      7 -> Seven
      8 -> All

shipStockedBitsResult :: StockedBitsLen -> StockedBitsLen -> Word8 -> (Word8,StockedBitsLen) -- 左は得られる値、右は残るビット数
shipStockedBitsResult rest n val
  | rest < n = shipStockedBitsResult rest rest val
  | otherwise =
    let val'  = val .&. (_stockedValueMask rest)
        rest' = fromEnum n
        n'    = fromEnum n
        slen' = rest' - n'
    in (val' `shiftR` slen', toEnum $ slen')


class (ReadonlyInfo a, BytesHolderIO a) => Class a where
  -- please implement
  syncByte  :: a -> Word8
  new       :: String -> IO a
--  isEOF     :: a -> IO Bool
  hGet      :: (Integral i) => a -> i -> IO (ByteString,a)
  hSeek     :: (Integral i) => a -> i -> IO a
  syncIO    :: a -> IO a
  loaded    :: a -> Word8
  stockedBitsLen :: a -> StockedBitsLen
  getBits   :: (Integral i) => a -> i -> IO (Word64, a)
  ----- 

  stockedValueMask :: a -> Word8
  stockedValueMask = _stockedValueMask . stockedBitsLen

  stockedBitsNumLen :: (Num b) => a -> b
  stockedBitsNumLen = _stockedBitsNumLen . stockedBitsLen

  stockedValue :: a -> Word8
  stockedValue x = (loaded x) .&. (stockedValueMask x)

  addBytes :: (Integral i) => a -> i -> Word64 -> IO (Word64,a)
  addBytes fh byteslen curr
    | byteslen < 1 = return (curr,fh)
    | otherwise = do
        (bytes,fh2) <- getBytes fh byteslen
        return $ (\x -> (x,fh2)) $ foldl (\e rtn -> (rtn `shiftR` 8) .|. e) 0 $ Prelude.map (fromInteger . toInteger) $ BS.unpack bytes

  getReadonlyInfo :: a -> ReadonlyData
  getReadonlyInfo x = MkReadonlyData { __pos = pos x, __filesize = filesize x }
 
  getBytes :: (Integral i) => a -> i -> IO (ByteString, a)
  getBytes = TS.FileHandle.hGet
      
data Data = MkData {
  _handle    :: Handle,
  _pos       :: Word64, -- bits
  _filesize  :: BytesLen,
  _loaded    :: Word8,
  _stockedBitsLen :: StockedBitsLen,
  _bytesCounter :: BytesLen
  }

instance BytesCounter Data where
  getBytesCounter     = _bytesCounter 
  resetBytesCounter x = x {_bytesCounter = 0 }

instance ReadonlyInfo Data where
  pos = _pos
  filesize = _filesize


-- private method
_addPos :: Data -> Word64 -> Data
_addPos x i = x { _pos = i + (_pos x),  _bytesCounter = i + (_bytesCounter x) }

class ToWord64 a where
  toWord64 :: a -> Word64

instance ToWord64 Word8 where
  toWord64 = fromInteger . toInteger

instance BytesHolderIO Data where
  getBytesIO = getBytes
  getBitsIO = getBits
  isEOF = hIsEOF . _handle  

instance Class Data where
  loaded = _loaded
  stockedBitsLen = _stockedBitsLen
  new filepath = do
    h <- openFile filepath ReadMode
    filesize <- return . fromInteger =<< hFileSize h
    return MkData {
      _filesize  = filesize,
      _pos       = 0,
      _handle    = h,
      _loaded    = 0,
      _bytesCounter = 0,
      _stockedBitsLen = Zero
      }
      
  syncByte x = 0x47
  
--  hGet :: (Integral i) => a -> i -> IO (ByteString,a)
  hGet x i
    | i < 1 = return (BS.empty, x)
    | otherwise = 
      let i1 = (fromInteger $ toInteger i) :: Int
          i2 = (fromInteger $ toInteger i) :: Word64
      in BS.hGet (_handle x) i1  >>= (\bytes -> return (bytes, _addPos (x {_loaded = BS.last bytes, _stockedBitsLen = Zero}) i2))
  
  hSeek x i =  return . snd =<< TS.FileHandle.hGet x i

  syncIO fh
    | (_loaded fh) == (syncByte fh) = return fh
    | otherwise = do
        isEOF' <- isEOF fh
        if isEOF'
          then return fh
          else
          do
            (bytes,fh2) <- getBytes fh 1
            x <- return $ Data.ByteString.Lazy.head bytes
            if x == syncByte fh2
              then return fh2
              else syncIO fh2
  getBits fh bitslen
    | bitslen < 1 = return (0,fh)
    | otherwise =
        let bitslen'           = toInteger bitslen                :: Integer
            stockedlen         = toInteger (stockedBitsNumLen fh) :: Integer
            curr               = (stockedValue fh)                :: Word8
            (needloadlen,rest) = if bitslen' > stockedlen then ((bitslen' - stockedlen + 8) `divMod` 8) else (0,stockedlen - bitslen') :: (Integer,Integer)
            load_for_rest_len  = if needloadlen > 0 && rest > 0 then 1 else 0 :: Integer
            first_load_len     = needloadlen - load_for_rest_len :: Integer
            rest'              = fromInteger rest
        in
          do
            (v2,fh2) <- addBytes fh first_load_len $ toWord64 curr
            (_ ,fh3) <- getBytes fh load_for_rest_len
            fh4 <- return $ fh3 { _stockedBitsLen = _numToStockedBitsLen $ fromInteger rest }
            v4  <- return $ (v2 `shiftL` rest') .|. (toWord64 $ stockedValue fh4)
            return (v4,fh4)
