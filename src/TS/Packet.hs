{-# LANGUAGE FlexibleInstances #-}
module TS.Packet where

import qualified TS.Packet.Header as Header
import qualified TS.Packet.Body   as Body
import qualified TS.FileHandle    as FH

import Data.Word(Word64, Word32, Word16, Word8)
import qualified Data.ByteString.Lazy as BS
import Common(BytesLen,EmptyExist(..),PID,TableID)
import qualified Parser
import qualified Data.Vector as V

type FileHandle = FH.Data

bytesLen :: BytesLen
bytesLen = 188

_TSPAYLOADMAX :: Int
_TSPAYLOADMAX=184

--class PacketHolder a where
--  add :: a -> Data -> a

class (Header.Class t, Body.Class t) => Class t where
  -- please implement
  mkEOF :: t
  mkOK  :: Header.Data -> Body.Data -> t
  isEOF :: t -> Bool
  isOK  :: t -> Bool
  ----------

  -- 引数のByteStringは同期用の先頭バイトは削られているもの
  fromByteString :: BS.ByteString -> t
  fromByteString bytes =
    let plen'     = fromInteger $ toInteger $ bytesLen - 1
        byteslen' = BS.length bytes
        bytes'    = BS.take plen' bytes
        header    = Header.parse $ BS.take 3 bytes'
        body      = BS.drop 3 bytes'
    in
      if byteslen' < plen'
      then mkEOF
      else mkOK header body

  read :: FileHandle -> IO (t,(BS.ByteString,FileHandle))
  read h = do
    h' <- FH.syncIO h
    res@(bytes,h'') <- FH.getBytes h' bytesLen
    return (fromByteString bytes,(bytes,h''))
  
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

class (EmptyExist a, Parser.HasParser a) => FromPackets a where
  table_ids :: (a -> b) -> [TableID]
  pids      :: (a -> b) -> [PID]
  
  isMatch :: (a -> b) -> Data -> Bool
  isMatch f packet = let pid = Header.pid packet in any (==pid) (pids f)
  
  fromPackets :: (a -> b) -> V.Vector Data -> V.Vector a
  fromPackets f =
    let table_ids' = table_ids f
    in fst . Parser.parseMulti . (V.foldl BS.append BS.empty) . (V.map Body.payload) . V.filter (isMatch f)

