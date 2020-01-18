module Descriptor.Link.ErtNodeInfo (
  Class(..)
  ,Data
  ) where

import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)

class Class a where
  information_provider_id :: a -> Word16
  event_relation_id       :: a -> Word16
  
data Data = MkData {
  _information_provider_id :: Word16,
  _event_relation_id :: Word16
  } -- 0x06

instance Class Data where
  information_provider_id = _information_provider_id
  event_relation_id       = _event_relation_id
