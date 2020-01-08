module Descriptor.BouquetName where
import Descriptor.Common(Base,HasName)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasName a) => BouquetName a where
