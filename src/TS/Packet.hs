{-# LANGUAGE FlexibleInstances #-}
module TS.Packet where

import qualified TS.Packet.Header as Header
import qualified TS.Packet.Body   as Body
--import qualified TS.FileHandle    as FH

import qualified BytesReader

import Data.Word(Word64, Word32, Word16, Word8)
import qualified Data.ByteString.Lazy as BS
import Common(BytesLen,EmptyExist(..),PID,TableID)
import qualified Parser
import qualified Data.Vector as V
import Data.Int(Int64)
import qualified BytesReader.HolderIO as HolderIO
import qualified TS.FileHandle as FH

-- sync byteを含めた長さ
bytesLen :: BytesLen
bytesLen = 188

class (Header.Class t, Body.Class t) => Class t where
  -- please implement
  mkEOF :: t
  mkOK  :: Header.Data -> Body.Data -> t
  isEOF :: t -> Bool
  isOK  :: t -> Bool
  ----------

  -- 引数のByteStringは同期用の先頭バイトは削られているものを想定している
  fromByteString :: BS.ByteString -> t
  fromByteString bytes =
    let plen'     = toInteger $ bytesLen - 1    :: Integer
        plen''    = fromInteger plen'           :: Int64
        byteslen' = toInteger $ BS.length bytes :: Integer
        bytes'    = BS.take plen'' bytes
        header    = Header.parse $ BS.take 3 bytes'
        body      = BS.drop 3 bytes'
    in
      if byteslen' < plen'
      then mkEOF
      else mkOK header body

  read :: (FH.Class fh) => fh -> IO (t,(BS.ByteString,fh))
  read h = do
    h' <- FH.syncIO h
    res@(bytes,h'') <- FH.getBytesIO h' (bytesLen - 1)
    return (fromByteString bytes,res)
  
data Data = MkData Header.Data Body.Data | EOF | Broken

instance EmptyExist Data where
  mkEmpty = MkData mkEmpty mkEmpty

instance Header.Class Data where
  header (MkData h _) = h
  header  _           = mkEmpty :: Header.Data  
  
instance Body.Class Data where
  header (MkData h _) = h
  body_data (MkData _ b) = b
  body_data _            = mkEmpty :: Body.Data  

instance Class Data where
  isEOF EOF = True
  isEOF _   = False
  isOK (MkData _ _) = True
  isOK _            = False

  mkEOF = EOF
  mkOK h b = MkData h b

-- class (EmptyExist a, Parser.Class a) => FromPackets a where
--   table_ids :: (a -> b) -> [TableID]
--   pids      :: (a -> b) -> [PID]
  
--   isMatch :: (a -> b) -> Data -> Bool
--   isMatch f packet = let pid = Header.pid packet in any (==pid) (pids f)
  
--   fromPackets :: (a -> b) -> V.Vector Data -> V.Vector a
--   fromPackets f = 
--     let table_ids' = table_ids f
--     in fst . Parser.parseMulti . (V.foldl BS.append BS.empty) . (V.map Body.payload) . V.filter (isMatch f)

-- class Holder a where
--   get :: a -> [Data]
