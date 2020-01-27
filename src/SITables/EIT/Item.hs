{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.EIT.Item(
  Data,
  Class(..),
  ) where

import qualified BytesReader.Base as BytesReaderBase
import qualified Parser.Result as Result
import Utils
import qualified Descriptor
import Data.Vector(Vector,toList,empty)
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.FromByteString as FromByteString
import qualified Data.ByteString.Lazy as BS

class Class a where
  event_id                :: a -> Word16
  start_time              :: a -> Word64
  duration                :: a -> Word32
  running_status          :: a -> Word8
  free_CA_mode            :: a -> Bool
  descriptors_loop_length :: a -> Word16
  descriptors             :: a -> [Descriptor.Data]

data Data = MkData {
  _event_id                :: Word16,
  _start_time              :: Word64,
  _duration                :: Word32,
  _running_status          :: Word8,
  _free_CA_mode            :: Bool,
  _descriptors_loop_length :: Word16,
  _descriptors             :: Vector Descriptor.Data
  } deriving (Show)

instance Class Data where
  event_id                = _event_id 
  start_time              = _start_time
  duration                = _duration
  running_status          = _running_status
  free_CA_mode            = _free_CA_mode
  descriptors_loop_length = _descriptors_loop_length
  descriptors             = toList . _descriptors

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty Data.Vector.empty

instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    let ((event_id -- 16
          ,w64 -- _start_time(40) _duration (24)
          ,w16 -- 3,1,12 :: _running_status, _free_CA_mode, _descriptors_loop_length
          ),bs0) = fromByteStringWithRest bs
          
        start_time              =            (w64 .&. 0xFFFFFFFFF000000) `shiftR` 24 
        duration                = toWord32 $ (w64 .&. 0x000000000FFFFFF) `shiftR`  0
        running_status          = toWord8  $ (w16 .&. 0xC000) `shiftR` 13
        free_CA_mode            = (/= 0)   $ (w16 .&. 0x1000) `shiftR` 12
        descriptors_loop_length =            (w16 .&. 0x0FFF) `shiftR`  0
        (bs1,rest)              = BS.splitAt (fromInteger $ toInteger descriptors_loop_length) bs0
        descriptors             = fromByteString bs1
          
        d = MkData {
          _event_id                = event_id                ,
          _start_time              = start_time              ,
          _duration                = duration                ,
          _running_status          = running_status          ,
          _free_CA_mode            = free_CA_mode            ,
          _descriptors_loop_length = descriptors_loop_length ,
          _descriptors             = descriptors             
          }
      in (d,rest)

-- _parseIOFlow :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow fh init = do
--   -- putStrLn "EIT.Item::_parseIOFlow"
--   getBitsIO_M fh [
--     (16, (\(v,d) -> d { _event_id                = fromWord64 v})),
--     (40, (\(v,d) -> d { _start_time              = fromWord64 v})),
--     (24, (\(v,d) -> d { _duration                = fromWord64 v})),
--     ( 3, (\(v,d) -> d { _running_status          = fromWord64 v})),
--     ( 1, (\(v,d) -> d { _free_CA_mode            = fromWord64 v})),
--     (12, (\(v,d) -> d { _descriptors_loop_length = fromWord64 v}))
--     ] init

-- instance Parser.Class Data where
--   parseIOFlow = 
--     flowStart |>>= _parseIOFlow
    
