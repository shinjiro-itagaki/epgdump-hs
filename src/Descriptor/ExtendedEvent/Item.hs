module Descriptor.ExtendedEvent.Item (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)

class Class a where
  item_description_length :: a -> Word8
  item_description_chars  :: a -> String
  item_length             :: a -> Word8
  item_chars              :: a -> String  
  
data Data = MkData {
  _item_description_length :: Word8,
  _item_description_chars  :: String,
  _item_length             :: Word8,
  _item_chars              :: String
  } deriving (Show)

instance Class Data where
  item_description_length = _item_description_length
  item_description_chars  = _item_description_chars
  item_length             = _item_length
  item_chars              = _item_chars
    
