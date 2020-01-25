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

  -- これを実行するとgetBitsIOのためにロードしたWord8を破棄してしまうので注意すること
  getBytesIO :: (Integral i) => a -> i -> IO (ByteString, a)
  cache      :: a -> ByteString
  clearCache :: a -> a
  loaded   :: a -> Word8    
  stockedBitsLen :: a -> StockedBitsLen.Data
  updateStockedBitsLen :: a -> StockedBitsLen.Data -> a
  -----

  hSeek :: (Integral i) => a -> i -> IO a
  hSeek x i =  return . snd =<< getBytesIO x i

  -- ビット単位でデータを取得するためにロードされているWord8の値から残っている値を読み取るためのマスク
  stockedValueMask :: a -> Word8
  stockedValueMask = StockedBitsLen.stockedValueMask . stockedBitsLen

  -- ビット単位でデータを取得するためにロードされているWord8のうち、何ビットが残っているかを示す値
  stockedBitsNumLen :: (Num b) => a -> b
  stockedBitsNumLen = StockedBitsLen.stockedBitsNumLen . stockedBitsLen

  -- ビット単位でデータを取得するためにロードされているWord8そのもの
  stockedValue :: a -> Word8
  stockedValue x = (loaded x) .&. (stockedValueMask x)

  -- i バイトだけデータをロードしてその値を引数で渡した値にシフト加算していく
  addBytes :: (Integral i) => a -> i -> Word64 -> IO (Word64,a)
  addBytes fh byteslen curr
    | byteslen < 1 = return (curr,fh)
    | otherwise = do
        (bytes,fh2) <- getBytesIO fh byteslen
        -- putStrLn $ ("addBytes=" ++) $ show $ BS.unpack bytes
        res <- return $ BS.foldl (\rtn e -> (rtn `shiftL` 8) .|. (fromInteger $ toInteger e)) (0 :: Word64) bytes
        -- putStrLn $ ("rtn of addBytes=" ++) $ show res
        return (res,fh2)

  getBitsIO  :: (Integral i) => a -> i -> IO (Word64, a)
  getBitsIO fh bitslen
    | bitslen < 1 = return (0,fh)
    | otherwise =
        let bitslen'           = toInteger bitslen                :: Integer -- 取得するビットの数
            stockedlen         = toInteger (stockedBitsNumLen fh) :: Integer -- ロードしたWord8のうち、現在残っているビット数
            curr               = (stockedValue fh)                :: Word8 -- ロードしたWord8のうち、現在残っているビット数が示す値
            (needloadlen,rest) = if bitslen' > stockedlen -- 残っているビット数以上に取得する場合
                                 then ((bitslen' - stockedlen) `divMod` 8) 
                                 else (0,stockedlen - bitslen') -- 新たにWord8をロードする必要はない
            load_for_rest_len  = if needloadlen > 0 && rest > 0 -- 中途半端に多いビットを取得するためにWord8をロードする必要があるか
                                 then 1
                                 else 0
            rest'              = fromInteger rest
        in
          do
            -- putStrLn $ ("bitslen'=" ++) $ show bitslen'
            -- putStrLn $ ("stockedlen=" ++) $ show stockedlen
            -- putStrLn $ ("curr=" ++) $ show curr
            -- putStrLn $ ("(needloadlen,rest)=" ++) $ show (needloadlen,rest)
            -- putStrLn $ ("load_for_rest_len=" ++) $ show load_for_rest_len
            -- putStrLn $ ("rest'=" ++) $ show rest'
            -- (\x -> putStrLn $ ("jjjjjj'=" ++) $ show $ fst x) =<< getBytesIO fh 1
            -- putStrLn $ ("counter=" ++) $ show $ Counter.getBytesCounter fh
            (v2,fh2) <- addBytes fh needloadlen $ toWord64 curr
            -- putStrLn $ ("counter=" ++) $ show $ Counter.getBytesCounter fh2            
            -- putStrLn $ ("v2'=" ++) $ show v2
            (_ ,fh3) <- getBytesIO fh2 load_for_rest_len
            -- putStrLn $ ("counter=" ++) $ show $ Counter.getBytesCounter fh3
            -- putStrLn "----"
            fh4 <- return $ updateStockedBitsLen fh3 $ StockedBitsLen.numToStockedBitsLen $ fromInteger rest -- 残りのビット数をセット
            v4  <- return $ (v2 `shiftL` rest') .|. (toWord64 $ stockedValue fh4)
            -- putStrLn $ ("v4'=" ++) $ show v4
            return (v4,fh4)
            -- return (19,fh4)
