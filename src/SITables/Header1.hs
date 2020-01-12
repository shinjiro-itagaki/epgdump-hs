{-# LANGUAGE FlexibleInstances #-}

module SITables.Header1 where
import Data.Word(Word64, Word32, Word16, Word8)
import Common(EmptyExist(..),BitsLen)
import Parser(HasParser(..),ParseConditionSymbol(..),FromValueCache(..),ValueCache)
class Class a where
  header1                     :: a -> Data
  table_id                    :: a -> Word8
  section_syntax_indicator    :: a -> Bool
  reserved_future_use         :: a -> Bool
  reserved1                   :: a -> Word8
  section_length              :: a -> Word16
  table_id                 = table_id                 . header1
  section_syntax_indicator = section_syntax_indicator . header1
  reserved_future_use      = reserved_future_use      . header1
  reserved1                = reserved1                . header1
  section_length           = section_length           . header1

data Data = MkData {
  _table_id                 :: Word8,
  _section_syntax_indicator :: Bool,
  _reserved_future_use      :: Bool,
  _reserved1                :: Word8,
  _section_length           :: Word16
  }

instance Class Data where
  header1                x = x
  table_id                 = _table_id 
  section_syntax_indicator = _section_syntax_indicator
  reserved_future_use      = _reserved_future_use
  reserved1                = _reserved1
  section_length           = _section_length

instance EmptyExist Data where
  mkEmpty = MkData {
    _table_id                 = mkEmpty,
    _section_syntax_indicator = mkEmpty,
    _reserved_future_use      = mkEmpty,
    _reserved1                = mkEmpty,
    _section_length           = mkEmpty
    }
  
data Symbol = TableID | SectionSyntaxIndicator | ReservedFutureUse | Reserved1 | SectionLength deriving (Eq,Enum,Bounded)

update :: Symbol -> ValueCache -> Data -> (Data,Maybe Symbol)
update TableID                v st = (st { _table_id                 = fromValueCache v },Nothing)
update SectionSyntaxIndicator v st = (st { _section_syntax_indicator = fromValueCache v },Nothing)
update ReservedFutureUse      v st = (st { _reserved_future_use      = fromValueCache v },Nothing)
update Reserved1              v st = (st { _reserved1                = fromValueCache v },Nothing)
update SectionLength          v st = (st { _section_length           = fromValueCache v },Nothing)

result :: Data -> Maybe Data
result x = Just x

instance ParseConditionSymbol Symbol where
  getLen TableID                = 8
  getLen SectionSyntaxIndicator = 1
  getLen ReservedFutureUse      = 1
  getLen Reserved1              = 2
  getLen SectionLength          =12

instance HasParser Data where
  parse = startParse update result

length :: BitsLen
length = bitsLength (allSymbols :: [Symbol])
