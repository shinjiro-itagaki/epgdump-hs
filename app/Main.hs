module Main where

import TS

import System.IO(openFile, IOMode(ReadMode))
import System.Environment (getArgs)
import Data.Word(Word64)
-- import Data.ByteString(hGet,length, ByteString,unpack, take, pack, append,map, head)
import Data.ByteString.Lazy(ByteString)
import qualified TS.Packet
import qualified TS.FileHandle as FileHandle

--_KB = 2 ^ 10
--_MB = _KB ^ 2

main :: IO ()
main = do
  args <- getArgs
  let filepath =  args !! 0
      counter = 0 :: Word64
  TS.each filepath counter action >>= (\counter -> putStr . ("count = " ++) $ show counter)

action :: TS.Packet.Data -> Word64 -> FileHandle.ReadonlyData -> ByteString -> IO (Bool,Word64)
action _ x _ _ = return (True,x+1)
    
