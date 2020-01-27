{-# LANGUAGE FlexibleInstances #-}

module SITables.Header2 where
import qualified Utils.EmptyExist as EmptyExist
import qualified BytesReader.Base as BytesReaderBase
import qualified Parser.Result as Result
import qualified Utils.FromByteString as FromByteString
import qualified Utils.EmptyExist as EmptyExist
import Utils

class (Show a) => Class a where
  header2                :: a -> Data
  
  reserved2              :: a -> Word8
  reserved2              = reserved2              . header2
  
  version_number         :: a -> Word8
  version_number         = version_number         . header2
  
  current_next_indicator :: a -> Bool
  current_next_indicator = current_next_indicator . header2
  
  section_number         :: a -> Word8
  section_number         = section_number         . header2
  
  last_section_number    :: a -> Word8
  last_section_number    = last_section_number    . header2

data Data = MkData {
  _reserved2              :: Word8,
  _version_number         :: Word8,
  _current_next_indicator :: Bool,
  _section_number         :: Word8,
  _last_section_number    :: Word8
  } deriving (Show)

instance Class Data where
  header2              x = x
  reserved2              = _reserved2
  version_number         = _version_number 
  current_next_indicator = _current_next_indicator
  section_number         = _section_number
  last_section_number    = _last_section_number


instance EmptyExist.Class Data where
  mkEmpty = MkData {
    _reserved2                = mkEmpty,
    _version_number           = mkEmpty,
    _current_next_indicator   = mkEmpty,
    _section_number           = mkEmpty,
    _last_section_number      = mkEmpty
    }

instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    let (w8                  ,bs0) = fromByteStringWithRest bs
        reserved2                  =          (`shiftR` 6) $ w8 .&. 0xC0
        version_number             =          (`shiftR` 5) $ w8 .&. 0x3E
        current_next_indicator     = (/= 0) $ (`shiftR` 0) $ w8 .&. 0x01
        (section_number      ,bs1) = fromByteStringWithRest bs0
        (last_section_number, bs2) = fromByteStringWithRest bs1
        d = MkData{
          _reserved2                = reserved2,
          _version_number           = version_number,
          _current_next_indicator   = current_next_indicator,
          _section_number           = section_number,
          _last_section_number      = last_section_number
          }
    in (d,bs2)
