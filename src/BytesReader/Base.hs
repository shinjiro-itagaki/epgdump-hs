module BytesReader.Base where

import qualified BytesReader.Counter as Counter
import Data.Word(Word64, Word32, Word16, Word8)
import Common(ByteString,BytesLen)
import qualified BytesReader.StockedBitsLen as StockedBitsLen
import qualified Data.ByteString.Lazy as BS
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import Utils.ToWord64(toWord64)

class (Counter.Class a) => Class a where
  ----
  -- please implement
  ----
  isEOF      :: a -> IO Bool
  getBytesIO :: (Integral i) => a -> i -> IO (ByteString, a)
  loaded     :: a -> Word8
  -----

  hSeek :: (Integral i) => a -> i -> IO a
  hSeek x i =  return . snd =<< getBytesIO x i

  -- i バイトだけデータをロードしてその値を引数で渡した値にシフト加算していく
  addBytes :: (Integral i) => a -> i -> Word64 -> IO (Word64,a)
  addBytes fh byteslen curr
    | byteslen < 1 = return (curr,fh)
    | otherwise = do
        (bytes,fh2) <- getBytesIO fh byteslen
        res <- return $ BS.foldl (\rtn e -> (rtn `shiftL` 8) .|. (fromInteger $ toInteger e)) (0 :: Word64) bytes
        return (res,fh2)
