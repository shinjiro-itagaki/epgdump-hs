{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module BytesReader(
  Class(..)
  ,Data
  ,new
  ) where
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Lazy(pack,unpack,null,uncons,empty)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import Data.Char(chr)
import Common(ByteString,BytesLen)
import Data.ByteString.Lazy(head,last,unpack)
import qualified Data.ByteString.Lazy as BS
import qualified BytesReader.Status as Status
import qualified BytesReader.Handler as Handler
import qualified BytesReader.Counter as Counter
import qualified BytesReader.HolderIO as HolderIO
import qualified BytesReader.StockedBitsLen as StockedBitsLen

-- ロード済みの在庫Word8のうち何ビットが残っているかを示すフラグ
-- 在庫の消費は左側ビットが優先

class (HolderIO.Class a, Status.Class a) => Class a where
  -- please implement
  loaded   :: a -> Word8
  stockedBitsLen :: a -> StockedBitsLen.Data
  getBits  :: (Integral i) => a -> i -> IO (Word64, a)
  getBytes :: (Integral i) => a -> i -> IO (ByteString, a)
  pos      :: a -> BytesLen
  size     :: a -> BytesLen
  -----
  
  hSeek :: (Integral i) => a -> i -> IO a
  hSeek x i =  return . snd =<< BytesReader.getBytes x i
  
  syncIO :: a -> Word8 ->IO a
  syncIO fh syncByte
    | (loaded fh) == syncByte = return fh
    | otherwise = do
        isEOF' <- HolderIO.isEOF $ fh
        if isEOF'
          then return fh
          else
          do
            (bytes,fh2) <- getBytes fh 1
            x <- return $ Data.ByteString.Lazy.head bytes
            if x == syncByte
              then return fh2
              else syncIO fh2 syncByte

  stockedValueMask :: a -> Word8
  stockedValueMask = StockedBitsLen.stockedValueMask . stockedBitsLen

  stockedBitsNumLen :: (Num b) => a -> b
  stockedBitsNumLen = StockedBitsLen.stockedBitsNumLen . stockedBitsLen

  stockedValue :: a -> Word8
  stockedValue x = (loaded x) .&. (stockedValueMask x)

  addBytes :: (Integral i) => a -> i -> Word64 -> IO (Word64,a)
  addBytes fh byteslen curr
    | byteslen < 1 = return (curr,fh)
    | otherwise = do
        (bytes,fh2) <- getBytes fh byteslen
        return $ (\x -> (x,fh2)) $ foldl (\e rtn -> (rtn `shiftR` 8) .|. e) 0 $ Prelude.map (fromInteger . toInteger) $ BS.unpack bytes

  getStatus :: a -> Status.Data
  getStatus x = Status.new (pos x) (size x)

data (Handler.Class h) => Data h = MkData {
  _handle         :: h,
  _pos            :: Word64, -- bits
  _size           :: BytesLen,
  _loaded         :: Word8,
  _stockedBitsLen :: StockedBitsLen.Data,
  _bytesCounter   :: BytesLen
  }

new :: (Handler.Class h) => h -> IO (Data h)
new h = do
  size <- Handler.size h
  return MkData {
    _handle         = h,
    _pos            = 0,
    _size           = size,
    _loaded         = 0,
    _bytesCounter   = 0,
    _stockedBitsLen = StockedBitsLen.Zero
    }


instance (Handler.Class h) => Status.Class (Data h) where
  pos = _pos
  size = _size
  
instance (Handler.Class h) => Counter.Class (Data h) where
  getBytesCounter     = _bytesCounter 
  resetBytesCounter x = x {_bytesCounter = 0 }

-- private method
_addPos :: (Handler.Class h) => (Data h) -> Word64 -> (Data h)
_addPos x i = x { _pos = i + (_pos x),  _bytesCounter = i + (_bytesCounter x) }

class ToWord64 a where
  toWord64 :: a -> Word64

instance ToWord64 Word8 where
  toWord64 = fromInteger . toInteger

instance (Handler.Class h) => HolderIO.Class (Data h) where
  isEOF = Handler.isEOF . _handle  
  getBytesIO = getBytes
  getBitsIO = getBits

instance (Handler.Class h) => Class (Data h) where
  pos            = _pos
  size           = _size
  loaded         = _loaded
  stockedBitsLen = _stockedBitsLen
  getBytes x i
    | i < 1 = return (BS.empty, x)
    | otherwise = 
      let i1 = (fromInteger $ toInteger i) :: Int
          i2 = (fromInteger $ toInteger i) :: Word64
      in Handler.hGet (_handle x) i1  >>= (\bytes -> return (bytes, _addPos (x {_loaded = BS.last bytes, _stockedBitsLen = StockedBitsLen.Zero}) i2))

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
            fh4 <- return $ fh3 { _stockedBitsLen = StockedBitsLen.numToStockedBitsLen $ fromInteger rest }
            v4  <- return $ (v2 `shiftL` rest') .|. (toWord64 $ stockedValue fh4)
            return (v4,fh4)
