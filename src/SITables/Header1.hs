{-# LANGUAGE FlexibleInstances #-}

module SITables.Header1 where
import Data.Word(Word64, Word32, Word16, Word8)
-- import Descriptor
import Common(EmptyExist(..))
import Parser(ParseConditionSymbol(..),FromValueCache(..),ValueCache)

class Class a where
  table_id                    :: a -> Word8
  section_syntax_indicator    :: a -> Bool
  reserved_future_use         :: a -> Bool
  reserved1                   :: a -> Word8
  section_length              :: a -> Word16

data Data = MkData {
  _table_id                 :: Word8,
  _section_syntax_indicator :: Bool,
  _reserved_future_use      :: Bool,
  _reserved1                :: Word8,
  _section_length           :: Word16
  }

instance EmptyExist Data where
  mkEmpty = MkData {
    _table_id                 = mkEmpty,
    _section_syntax_indicator = mkEmpty,
    _reserved_future_use      = mkEmpty,
    _reserved1                = mkEmpty,
    _section_length           = mkEmpty
    }
  
data Symbol = TableID | SectionSyntaxIndicator | ReservedFutureUse | Reserved1 | SectionLength deriving (Eq,Enum,Bounded)

_stateUpdater :: Symbol -> ValueCache -> Data -> Data
_stateUpdater sym v st =
  case sym of
    TableID                -> st { _table_id                 = fromValueCache v }
    SectionSyntaxIndicator -> st { _section_syntax_indicator = fromValueCache v }
    ReservedFutureUse      -> st { _reserved_future_use      = fromValueCache v }
    Reserved1              -> st { _reserved1                = fromValueCache v }
    SectionLength          -> st { _section_length           = fromValueCache v }

instance ParseConditionSymbol Symbol where
  getLen TableID                = 8
  getLen SectionSyntaxIndicator = 1
  getLen ReservedFutureUse      = 1
  getLen Reserved1              = 2
  getLen SectionLength          =12
