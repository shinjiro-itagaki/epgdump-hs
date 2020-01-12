{-# LANGUAGE FlexibleInstances #-}
module TS.Packet.AdaptationField where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Bits(Bits(..))
import Data.ByteString.Lazy(ByteString)
import qualified TS.Packet.AdaptationField.OptionalFields1 as OptionalFields1

data Flags5 = MkFlags5 Bool Bool Bool Bool Bool

class Class a where
  adaptation_field_length              :: a -> Word8 -- 8
  discontinuity_indicator              :: a -> Bool -- 1
  random_access_indicator              :: a -> Bool -- 1
  elementary_stream_priority_indicator :: a -> Bool -- 1
  flags5                               :: a -> Flags5 -- 5
  optional_fields_1                    :: a -> OptionalFields1.Data 
  stuffing_bytes                       :: a -> ByteString -- ??

data Data = MkData {
  _adaptation_field_length              :: Word8, -- 8
  _discontinuity_indicator              :: Bool, -- 1
  _random_access_indicator              :: Bool, -- 1
  _elementary_stream_priority_indicator :: Bool, -- 1
  _flags5                               :: Flags5, -- 5
  _optional_fields_1                    :: OptionalFields1.Data,
  _stuffing_bytes                       :: ByteString -- ??
  }

instance Class Data where
  adaptation_field_length              = _adaptation_field_length
  discontinuity_indicator              = _discontinuity_indicator
  random_access_indicator              = _random_access_indicator
  elementary_stream_priority_indicator = _elementary_stream_priority_indicator
  flags5                               = _flags5
  optional_fields_1                    = _optional_fields_1
  stuffing_bytes                       = _stuffing_bytes
