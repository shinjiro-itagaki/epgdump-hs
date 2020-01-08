module Descriptor.BoardInformation where
import Descriptor.Common(Base,HasTitle, HasTextAndLen)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasTitle a, HasTextAndLen a) => BoardInformation a where
--  title :: a -> String
--  title_length :: a -> Word8


