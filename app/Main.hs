module Main where

import TS

import System.IO(openFile, IOMode(ReadMode))
import System.Environment (getArgs)
import Common(BytesLen)
import qualified Data.ByteString.Lazy as BS
import qualified TS.Packet
import qualified TS.FileHandle as FileHandle
import Data.Word(Word64,Word8)
import Data.Bits(shiftL,shiftR)

--_KB = 2 ^ 10
--_MB = _KB ^ 2

data Counter = MkCounter {
  
                         }

main :: IO ()
main = do
  args <- getArgs
  let filepath =  args !! 0
      counter = 0 :: BytesLen
--  fh <- FileHandle.new filepath   
--  test fh 0 >>= putStrLn . show
  TS.each filepath counter action >>= (\counter -> putStrLn . ("count of packets is = " ++) $ show counter)

action :: TS.Packet.Data -> BytesLen -> FileHandle.ReadonlyData -> BS.ByteString -> IO (Bool,BytesLen)
action _ x info _ = do
  let newx = x+1
  if newx `mod` 20000 == 0
    then putStrLn $ show $ FileHandle.progress_percent info
    else return ()
  return (True,newx)

test :: FileHandle.Data -> Word64 -> IO Word64
test fh i = do
  fh2 <- FileHandle.syncIO fh
  (bs,fh3) <- FileHandle.hGet fh2 187
  if BS.length bs < 1
    then return i
    else do
--      test fh $ (\x -> if x == 0 then i+1 else i) $ (`mod` 188) $ foldl (+) 0 $ map (`shiftR` 1) $ BS.unpack bs
--      test fh $ (\x -> if x == 0 then i+1 else i) $ (`mod` 188) $ foldl (+) 0 $ BS.unpack bs
        test fh $ (\x -> if x == 0 then i+1 else i) $ (`mod` 10) $ BS.foldl max (0::Word8) $ BS.map (`shiftR` 1) bs
      
