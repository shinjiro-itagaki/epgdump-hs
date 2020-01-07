module Main where

import TS(
  FileHandle(tspackets,syncByte)
  ,mkFileHandle
  ,syncIO
  ,isEOF
  ,TSpacket
  ,TSpacketTypeable(..)
  ,eachTSpackets
  )

import System.IO(openFile, IOMode(ReadMode))
import System.Environment (getArgs)
import Data.Word(Word64)
-- import Data.ByteString(hGet,length, ByteString,unpack, take, pack, append,map, head)
import Data.ByteString(ByteString)

--_KB = 2 ^ 10
--_MB = _KB ^ 2

main :: IO ()
main = do
  args <- getArgs
  let filepath =  args !! 0
  handle <- openFile filepath ReadMode
  eachTSpackets (mkFileHandle handle) 0 isSDT impl >>= (\(_,counter) -> putStr . ("count = " ++) $ show counter)
  where
    impl :: TSpacket -> FileHandle -> Word64 -> ByteString -> IO (Word64, Bool)
    impl _ _ x _ = do
      -- if x == 1 || x `mod` 100 == 0 then putStr $ show x else return ()
      return (x+1, True)
