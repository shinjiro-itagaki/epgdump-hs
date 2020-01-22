module Descriptor.Link.StoredContentInfo (
  Class(..)
  ,Data
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)

class Class a where
  uri :: a -> String

data Data = MkData {
  _uri :: String
  } deriving (Show) -- 0x07

instance Class Data where
  uri = _uri
