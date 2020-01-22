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
import Common(EmptyExist(..))
import SITables(Callbacks(..))

--_KB = 2 ^ 10
--_MB = _KB ^ 2

data PacketCounter = MkPacketCounter {
  _packet_total   :: Word64,
  _has_adaptation :: Word64,
  _no_adaptation  :: Word64
  } deriving (Show)

instance EmptyExist PacketCounter where
  mkEmpty = MkPacketCounter {
    _packet_total   = 0,
    _has_adaptation = 0,
    _no_adaptation  = 0
    }

data TableCounter = MkTableCounter {
  _table_total :: Word64,
  _bat  :: Word64,
  _bit  :: Word64,
  _eit  :: Word64,
  _ldt  :: Word64,
  _nbit :: Word64,
  _nit  :: Word64,
  _pcat :: Word64,
  _rst  :: Word64,
  _sdt  :: Word64,
  _st   :: Word64,
  _tdt  :: Word64,
  _tot  :: Word64,
  _unm  :: Word64,
  _err  :: Word64
  } deriving (Show)

getTableCounterTotal :: TableCounter -> Word64
getTableCounterTotal x = sum [
  _bat  x,
  _bit  x,
  _eit  x,
  _ldt  x,
  _nbit x,
  _nit  x,
  _pcat x,
  _rst  x,
  _sdt  x,
  _st   x,
  _tdt  x,
  _tot  x,
  _unm  x,
  _err  x
  ]

mkTableCounterCallbacks :: Callbacks TableCounter
mkTableCounterCallbacks = MkCallbacks {
  _cb_BAT  = Just $ (\ _ x -> (return $ x {_bat  = 1 + (_bat  x)}) >>= action_count_tables),
  _cb_BIT  = Just $ (\ _ x -> (return $ x {_bit  = 1 + (_bit  x)}) >>= action_count_tables),
  _cb_EIT  = Just $ (\ _ x -> (return $ x {_eit  = 1 + (_eit  x)}) >>= action_count_tables),
  _cb_LDT  = Just $ (\ _ x -> (return $ x {_ldt  = 1 + (_ldt  x)}) >>= action_count_tables),
  _cb_NBIT = Just $ (\ _ x -> (return $ x {_nbit = 1 + (_nbit x)}) >>= action_count_tables),
  _cb_NIT  = Just $ (\ _ x -> (return $ x {_nit  = 1 + (_nit  x)}) >>= action_count_tables),
  _cb_PCAT = Just $ (\ _ x -> (return $ x {_pcat = 1 + (_pcat x)}) >>= action_count_tables),
  _cb_RST  = Just $ (\ _ x -> (return $ x {_rst  = 1 + (_rst  x)}) >>= action_count_tables),
  _cb_SDT  = Just $ (\ _ x -> (return $ x {_sdt  = 1 + (_sdt  x)}) >>= action_count_tables),
  _cb_ST   = Just $ (\ _ x -> (return $ x {_st   = 1 + (_st   x)}) >>= action_count_tables),
  _cb_TDT  = Just $ (\ _ x -> (return $ x {_tdt  = 1 + (_tdt  x)}) >>= action_count_tables),
  _cb_TOT  = Just $ (\ _ x -> (return $ x {_tot  = 1 + (_tot  x)}) >>= action_count_tables),
  _cb_UNM  = Just $ (\ _ x -> (return $ x {_unm  = 1 + (_unm  x)}) >>= action_count_tables),
  _cb_ERR  = Just $ (\ _ x -> (return $ x {_err  = 1 + (_err  x)}) >>= action_count_tables)   
  }

instance EmptyExist TableCounter where
  mkEmpty = MkTableCounter {
    _table_total = 0,
    _bat  = 0,
    _bit  = 0,
    _eit  = 0,
    _ldt  = 0,
    _nbit = 0,
    _nit  = 0,
    _pcat = 0,
    _rst  = 0,
    _sdt  = 0,
    _st   = 0,
    _tdt  = 0,
    _tot  = 0,
    _unm  = 0,
    _err  = 0
    }

addToPacketCounter :: PacketCounter -> TS.Packet.Data -> PacketCounter
addToPacketCounter c p
  | Header.has_adaptation_field p = c {_packet_total = (_packet_total c) + 1, _has_adaptation = (_has_adaptation c) + 1}
  | otherwise                     = c {_packet_total = (_packet_total c) + 1, _no_adaptation  = (_no_adaptation  c) + 1}

main :: IO ()
main = do
  args <- getArgs
  let filepath =  args !! 0
      counter = 0 :: BytesLen
      -- action_count_packets :: TS.Packet.Data -> PacketCounter -> FileHandle.ReadonlyData -> BS.ByteString -> IO (Bool,PacketCounter)
      action_count_packets' = Just (\ p c fhrd _ -> action_count_packets p c fhrd)
--  TS.eachPacket filepath mkEmpty action_count_packets >>= (\counter -> putStrLn . ("count of packets is = " ++) $ show counter)
  TS.eachTable filepath mkTableCounterCallbacks mkEmpty {- action_count_packets' -} >>= (\counter -> putStrLn . ("count of packets is = " ++) $ show counter)  

action_count_tables :: TableCounter -> IO (Bool,TableCounter)
action_count_tables x = do
  let total = getTableCounterTotal x
  if total `mod` 100 == 0
    then putStrLn $ show x 
    else return ()
  return (True,x { _table_total = total})


action_count_packets :: TS.Packet.Data -> PacketCounter -> FileHandle.ReadonlyData -> BS.ByteString -> IO (Bool,PacketCounter)
action_count_packets p x info _ = do
  let newx = addToPacketCounter x p
  if (_packet_total newx) `mod` 20000 == 0
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
      
