{-# LANGUAGE FlexibleInstances #-}
module TS.Packet where

import qualified TS.Packet.Header as Header
import qualified TS.Packet.Body   as Body
import qualified TS.FileHandle    as FH

import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString.Lazy(ByteString,length,take,drop)
import Common(BytesLen,EmptyExist(..))
import qualified Parser

type FileHandle = FH.Data

bytesLen :: BytesLen
bytesLen = 188

_TSPAYLOADMAX :: Int
_TSPAYLOADMAX=184

class PacketHolder a where
  add :: a -> Data -> a

class (Header.Class t, Body.Class t) => Class t where
  -- please implement
--  header :: t -> Header.Data
--  body   :: t -> Body.Data
  mkEOF :: t
  mkOK  :: Header.Data -> Body.Data -> t
  isEOF :: t -> Bool
  isOK  :: t -> Bool
  ----------

  typeNum :: t -> Word16
  typeNum = Header.pid . Header.header
  
  isNIT :: t -> Bool
  isNIT = (0x10==) . typeNum
  
  isSDT :: t -> Bool
  isSDT = (0x11==) . typeNum
  
  isEIT :: t -> Bool
  isEIT = (0x12==) . typeNum
  
  isRST :: t -> Bool
  isRST = (0x13==) . typeNum
  
  isTDT :: t -> Bool
  isTDT = (0x14==) . typeNum
  
  isSDTT:: t -> Bool
  isSDTT= (\x -> x == 0x23 || x == 0x28) . typeNum
  
  isBIT :: t -> Bool
  isBIT = (0x24==) . typeNum

  -- 引数のByteStringは同期用の先頭バイトは削られているもの
  fromByteString :: ByteString -> t
  fromByteString bytes =
    let plen'     = fromInteger $ toInteger $ bytesLen - 1
        byteslen' = Data.ByteString.Lazy.length bytes
        bytes'    = Data.ByteString.Lazy.take plen' bytes
        header    = Header.parse $ Data.ByteString.Lazy.take 3 bytes'
        body      = Body.parse $ Data.ByteString.Lazy.drop 3 bytes'
    in
      if byteslen' < plen'
      then mkEOF
      else mkOK header body

  read :: FileHandle -> IO (t,(ByteString,FileHandle))
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
  body (MkData _ b) = b
  body _            = mkEmpty :: Body.Data  

instance Class Data where
  isEOF EOF = True
  isEOF _   = False
  isOK (MkData _ _) = True
  isOK _            = False

  mkEOF = EOF
  mkOK h b = MkData h b
