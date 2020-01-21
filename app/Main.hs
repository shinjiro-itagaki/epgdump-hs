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
import qualified TS.Packet.AdaptationField as AdaptationField
import qualified TS.Packet.Header as Header

--_KB = 2 ^ 10
--_MB = _KB ^ 2

data Counter = MkCounter {
  _total          :: Word64,
  _has_adaptation :: Word64,
  _no_adaptation  :: Word64
  } deriving (Show)

newCounter = MkCounter {
  _total          = 0,
  _has_adaptation = 0,
  _no_adaptation  = 0
  }

addToCounter :: Counter -> TS.Packet.Data -> Counter
addToCounter c p
  | Header.has_adaptation_field p = c {_total = (_total c) + 1, _has_adaptation = (_has_adaptation c) + 1}
  | otherwise                     = c {_total = (_total c) + 1, _no_adaptation  = (_no_adaptation  c) + 1}

main :: IO ()
main = do
  args <- getArgs
  let filepath =  args !! 0
      counter = 0 :: BytesLen
  TS.eachPacket filepath newCounter action >>= (\counter -> putStrLn . ("count of packets is = " ++) $ show counter)

action :: TS.Packet.Data -> Counter -> FileHandle.ReadonlyData -> BS.ByteString -> IO (Bool,Counter)
action p x info _ = do
  let newx = addToCounter x p
  if (_total newx) `mod` 20000 == 0
    then putStrLn $ show $ FileHandle.progress_percent info
    else return ()
  return (True,newx)

test :: FileHandle.Data -> Word64 -> IO Word64
test fh i = do
  fh2 <- FileHandle.syncIO fh
  (bs,fh3) <- FileHandle.getBytesIO fh2 187
  if BS.length bs < 1
    then return i
    else do
--      test fh $ (\x -> if x == 0 then i+1 else i) $ (`mod` 188) $ foldl (+) 0 $ map (`shiftR` 1) $ BS.unpack bs
--      test fh $ (\x -> if x == 0 then i+1 else i) $ (`mod` 188) $ foldl (+) 0 $ BS.unpack bs
        test fh $ (\x -> if x == 0 then i+1 else i) $ (`mod` 10) $ BS.foldl max (0::Word8) $ BS.map (`shiftR` 1) bs
      
