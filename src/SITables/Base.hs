{-# LANGUAGE FlexibleInstances #-}

module SITables.Base where
import qualified BytesReader
import qualified BytesReader.Base as BytesReaderBase
import qualified BytesReader.Status as BytesReaderStatus
import qualified SITables.Header1 as Header1
import qualified SITables.Footer as Footer
import qualified Parser.Result as Result
import qualified Data.ByteString.Lazy as BS
import qualified Utils.FromByteString as FromByteString
import qualified Utils.EmptyExist as EmptyExist
import Utils
import qualified Data.Digest.CRC32 as CRC32
import qualified Utils.SITableIDs as SITableIDs

-- header1は必須
-- footerはないものもある
class (Header1.Class a, SITableIDs.Class a, Show a, EmptyExist.Class a) => Class a where
  footer                     :: a -> Maybe Footer.Data
  
  parseAfterHeader1 :: Header1.Data -> ByteString -> Result.Data a

  parse :: ByteString -> (Result.Data a,ByteString)
  parse bs =
    let (header1,bs0) = fromByteStringWithRest bs
        len = Header1.section_length header1
        (bs1,rest) = BS.splitAt (fromInteger $ toInteger len) bs0
    in (parseAfterHeader1 header1 bs1, rest)
        

  crc_length :: a -> BytesLen
  crc_length x = case footer x of
    Just footer -> Footer.crc_32_byteslen footer
    Nothing     -> 0
      
  section_length_without_crc :: a -> BytesLen
  section_length_without_crc x = (Header1.section_length x) - (crc_length x)

crcCheck :: ByteString -> Bool
crcCheck = (== 0) . CRC32.crc32
