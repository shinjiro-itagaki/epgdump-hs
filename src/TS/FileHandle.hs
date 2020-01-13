{-# LANGUAGE FlexibleInstances #-}
module TS.FileHandle where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Lazy(hGet,ByteString,head)
import System.IO(Handle,hIsEOF,SeekMode(..),hSeek,hFileSize,openFile, IOMode(ReadMode))
import Data.Vector(Vector,empty,toList)
import Common(BytesLen)
import Data.Ratio(Ratio)

class ReadonlyInfo a where
  pos :: a -> BytesLen
  filesize :: a -> BytesLen
  progress :: a -> Rational
  progress x = (toRational $ pos x) / (toRational $ filesize x)

  progress_percent :: (Fractional b) => a -> b
  progress_percent = fromRational . (* 100) . progress

data ReadonlyData = MkReadonlyData {
  __pos :: Word64,
  __filesize :: BytesLen
  }

instance ReadonlyInfo ReadonlyData where
  pos = __pos
  filesize = __filesize

class (ReadonlyInfo a) => Class a where
  -- please implement
  syncByte  :: a -> Word8
  new       :: String -> IO a
  updatePos :: a -> Word64 -> a
  isEOF     :: a -> IO Bool
  hGet      :: (Integral i, Num i) => a -> i -> IO ByteString
  hSeek     :: (Integral i, Num i) => a -> i -> IO a
  ----- 

  getReadonlyInfo :: a -> ReadonlyData
  getReadonlyInfo x = MkReadonlyData { __pos = pos x, __filesize = filesize x }
  
--  tspackets :: a -> [Data]
 
  getBytes :: (Integral i) => a -> i -> IO (ByteString, a)
  getBytes fh i = do
    bytes <- TS.FileHandle.hGet fh i'
    return (bytes, updatePos fh $ (+ i') $ pos fh)
    where
      i' = fromInteger $ toInteger i
      
  seek :: (Integral i) => a -> i -> IO a
  seek fh i = do
    TS.FileHandle.hSeek fh i
    return fh

  syncIO :: a -> IO a
  syncIO fh = do
    isEOF' <- isEOF fh
    if isEOF'
      then return fh
      else
      do
        (bytes,fh2) <- getBytes fh 1
        x <- return $ Data.ByteString.Lazy.head bytes
        if x == syncByte fh2
          then return fh2
          else syncIO fh2

data Data = MkData {
  _handle    :: Handle,
--  _tspackets :: Vector Data,
  _pos       :: Word64, -- bits
  _filesize  :: BytesLen
  }

instance ReadonlyInfo Data where
  pos = _pos
  filesize = _filesize

instance Class Data where
  isEOF = hIsEOF . _handle
  updatePos x v = x { _pos = v }
  
  new filepath = do
    h <- openFile filepath ReadMode
    filesize <- return . fromInteger =<< hFileSize h
    return MkData {
      _filesize  = filesize,
      _pos       = 0,
      _handle    = h
      }
      
  syncByte x = 0x47
--  filesize = (return . fromInteger =<<) . hFileSize . _handle

  hGet fh = Data.ByteString.Lazy.hGet (_handle fh) . fromInteger . toInteger
  
  hSeek fh i = do
    System.IO.hSeek (_handle fh) RelativeSeek i'
    return $ updatePos fh $ (+ i'') $ pos fh
    where
      i' = toInteger i
      i'' = fromInteger i'
