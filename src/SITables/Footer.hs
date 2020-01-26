{-# LANGUAGE FlexibleInstances #-}

module SITables.Footer where
import qualified BytesReader.Base as BytesReaderBase
import qualified Parser.Result as Result
import qualified Data.ByteString.Lazy as BS
import qualified Utils.FromByteString as FromByteString
import qualified Utils.EmptyExist as EmptyExist
import Utils

class (Show a) => Class a where
  footer :: a -> Data
  
  setFooter :: a -> Data -> a
  
  crc_32_bitslen :: a -> BitsLen
  crc_32_bitslen _ = 32

  crc_32_byteslen :: a -> BytesLen
  crc_32_byteslen = (`div` 8) . crc_32_bitslen

  crc_32 :: a -> Word32
  crc_32 = _crc_32 . footer
  
data Data = MkData {
  _crc_32 :: Word32
  } deriving (Show)

instance Class Data where
  setFooter x footer' = footer'
  footer  x = x
  crc_32    = _crc_32

instance EmptyExist.Class Data where
  mkEmpty = MkData {
    _crc_32 = mkEmpty
    }

instance FromByteString.Class Data where
  -- こちらは末尾の4bytesを取得する
  fromByteStringWithRest bs =
    let (rest,bs0) = BS.splitAt ((BS.length bs) - 4) bs
        d = MkData $ toWord32 bs0
    in (d,rest)
