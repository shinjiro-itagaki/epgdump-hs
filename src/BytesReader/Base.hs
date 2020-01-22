module BytesReader.Base where

import qualified BytesReader.Counter as Counter
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString,BytesLen)
import qualified BytesReader.StockedBitsLen as StockedBitsLen
import qualified Data.ByteString.Lazy as BS
import Data.Bits((.|.),(.&.),shiftL,shiftR)

class (Counter.Class a) => Class a where
  ----
  -- please implement
  ----
  isEOF      :: a -> IO Bool  
  getBytesIO :: (Integral i) => a -> i -> IO (ByteString, a)
  cache      :: a -> ByteString
  clearCache :: a -> a
  loaded   :: a -> Word8    
  stockedBitsLen :: a -> StockedBitsLen.Data
  updateStockedBitsLen :: a -> StockedBitsLen.Data -> a
  -----

  hSeek :: (Integral i) => a -> i -> IO a
  hSeek x i =  return . snd =<< getBytesIO x i
  
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
        (bytes,fh2) <- getBytesIO fh byteslen
        return $ (\x -> (x,fh2)) $ foldl (\e rtn -> (rtn `shiftR` 8) .|. e) 0 $ Prelude.map (fromInteger . toInteger) $ BS.unpack bytes

  getBitsIO  :: (Integral i) => a -> i -> IO (Word64, a)
  getBitsIO fh bitslen
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
            (_ ,fh3) <- getBytesIO fh load_for_rest_len
            fh4 <- return $ updateStockedBitsLen fh3 $ StockedBitsLen.numToStockedBitsLen $ fromInteger rest
            v4  <- return $ (v2 `shiftL` rest') .|. (toWord64 $ stockedValue fh4)
            return (v4,fh4)

  

class ToWord64 a where
  toWord64 :: a -> Word64

instance ToWord64 Word8 where
  toWord64 = fromInteger . toInteger

