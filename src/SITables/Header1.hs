{-# LANGUAGE FlexibleInstances #-}

module SITables.Header1 where
import qualified BytesReader.Base as BytesReaderBase
import qualified Parser.Result as Result
import Utils.FromWord64 hiding (Class)
import qualified BytesReader.Counter as Counter
import qualified Utils.FromByteString as FromByteString
import qualified Utils.EmptyExist as EmptyExist
import Utils

class Class a where
  header1                     :: a -> Data
  setHeader1                  :: a -> Data -> a

  table_id                 = table_id                 . header1  
  table_id                    :: a -> TableID
  
  section_syntax_indicator    :: a -> Bool
  section_syntax_indicator = section_syntax_indicator . header1

  reserved_future_use         :: a -> Bool
  reserved_future_use      = reserved_future_use      . header1
  
  reserved1                   :: a -> Word8
  reserved1                = reserved1                . header1
  
  section_length              :: a -> BytesLen
  section_length           = section_length           . header1

data Data = MkData {
  _table_id                 :: TableID,
  _section_syntax_indicator :: Bool,
  _reserved_future_use      :: Bool,
  _reserved1                :: Word8,
  _section_length           :: Word16
  } deriving (Show)

instance Class Data where
  header1                x = x
  setHeader1  self header1 = header1
  table_id                 = _table_id 
  section_syntax_indicator = _section_syntax_indicator
  reserved_future_use      = _reserved_future_use
  reserved1                = _reserved1
  section_length           = toWord64 . _section_length

instance EmptyExist.Class Data where
  mkEmpty = MkData {
    _table_id                 = mkEmpty,
    _section_syntax_indicator = mkEmpty,
    _reserved_future_use      = mkEmpty,
    _reserved1                = mkEmpty,
    _section_length           = mkEmpty
    }

instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    let (table_id,bs0) = fromByteStringWithRest bs
        (w16,     bs1) = fromByteStringWithRest bs0
        _ = w16 + (0 :: Word16)
        section_syntax_indicator = (/=0)   $ (`shiftR` 15) $ w16 .&. 0x8000
        reserved_future_use      = (/=0)   $ (`shiftR` 14) $ w16 .&. 0x4000
        reserved1                = toWord8 $ (`shiftR` 12) $ w16 .&. 0x3000
        section_length           =           (`shiftR`  0) $ w16 .&. 0x0FFF
        d = MkData {
          _table_id                 = table_id,
          _section_syntax_indicator = section_syntax_indicator,
          _reserved_future_use      = reserved_future_use,
          _reserved1                = reserved1,
          _section_length           = section_length
          }
    in (d,bs1)
