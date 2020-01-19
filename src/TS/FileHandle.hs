{-# LANGUAGE FlexibleInstances #-}
module TS.FileHandle (
  Data
  ,Class(..)
  ,ReadonlyData
  ,progress
  ,progress_percent
  ) where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Lazy(hGet,ByteString,head,last)
import qualified Data.ByteString.Lazy as BS
import System.IO(Handle,hIsEOF,SeekMode(..),hSeek,hFileSize,openFile, IOMode(ReadMode))
import Data.Vector(Vector,empty,toList)
import Common(BytesLen)
import qualified BytesReader
import qualified BytesReader.Handler as Handler
import qualified BytesReader.Counter as Counter
import qualified BytesReader.HolderIO as HolderIO
import Data.Ratio(Ratio)
import Data.Bits(shiftL,shiftR,(.|.),(.&.))
import qualified BytesReader
import qualified BytesReader.Status as Status

class (BytesReader.Class a) => Class a where
  -- please implement
  syncByte  :: a -> Word8
  new       :: String -> IO a
  syncIO    :: a -> IO a
  -----
  getBitsIO  :: (Integral i) => a -> i -> IO (Word64, a)
  getBitsIO = HolderIO.getBitsIO
  getBytesIO :: (Integral i) => a -> i -> IO (ByteString, a)
  getBytesIO = HolderIO.getBytesIO
  getBytesCounter   :: a -> BytesLen
  getBytesCounter = Counter.getBytesCounter
  resetBytesCounter :: a -> a
  resetBytesCounter = Counter.resetBytesCounter
  
  getReadonlyInfo :: a -> Status.Data  
  getReadonlyInfo = BytesReader.getStatus

  pos :: a -> BytesLen
  pos = BytesReader.pos
  
  size :: a -> BytesLen
  size = BytesReader.size

progress :: (Status.Class a) => a -> Rational
progress = Status.progress

progress_percent :: (Status.Class a, Fractional b) => a -> b
progress_percent = Status.progress_percent


instance Handler.Class Handle where
  hGet x = (BS.hGet x) . fromInteger . toInteger
  isEOF = hIsEOF
  size x = return . fromInteger =<< hFileSize x
  
type Data = BytesReader.Data Handle
type ReadonlyData = Status.Data

instance Class Data where
  syncByte _ = 0x47
  new filepath = do
    h <- openFile filepath ReadMode
    BytesReader.new h
      
  syncIO fh = BytesReader.syncIO fh (syncByte fh)
