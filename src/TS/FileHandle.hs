{-# LANGUAGE FlexibleInstances #-}
module TS.FileHandle where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Lazy(hGet,ByteString,head)
import System.IO(Handle,hIsEOF,SeekMode(..),hSeek,hFileSize,openFile, IOMode(ReadMode))
import Data.Vector(Vector,empty,toList)

class ReadonlyInfo a where
  pos :: a -> Word64 -- bits

data ReadonlyData = MkReadonlyData {
  __pos :: Word64 
  }

instance ReadonlyInfo ReadonlyData where
  pos = __pos

class (ReadonlyInfo a) => Class a where
  -- please implement
--  pos       :: a -> Word64 -- bits
  syncByte  :: a -> Word8
  new       :: String -> IO a
  updatePos :: a -> Word64 -> a
  isEOF     :: a -> IO Bool
  filesize  :: a -> IO Word64
  hGet      :: (Integral i, Num i) => a -> i -> IO ByteString
  hSeek     :: (Integral i, Num i) => a -> i -> IO a
  ----- 

  getReadonlyInfo :: a -> ReadonlyData
  getReadonlyInfo x = MkReadonlyData { __pos = pos x }
  
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
  _pos       :: Word64 -- bits
  }

instance ReadonlyInfo Data where
  pos = _pos  

instance Class Data where
  isEOF = hIsEOF . _handle
--  tspackets = toList . _tspackets
  updatePos x v = x { _pos = v }
  
  new filepath = do
    h <- openFile filepath ReadMode
    return MkData {
--      _tspackets = Data.Vector.empty,
      _pos       = 0,
      _handle    = h
      }
      
  syncByte x = 0x47
  filesize = (return . fromInteger =<<) . hFileSize . _handle

  hGet fh = Data.ByteString.Lazy.hGet (_handle fh) . fromInteger . toInteger
  
  hSeek fh i = do
    System.IO.hSeek (_handle fh) RelativeSeek i'
    return $ updatePos fh $ (+ i'') $ pos fh
    where
      i' = toInteger i
      i'' = fromInteger i'
