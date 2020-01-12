{-# LANGUAGE FlexibleInstances #-}
module TS.Packet.AdaptationField.OptionalFields1 where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Bits(Bits(..))
import Data.ByteString.Lazy(ByteString)
import qualified TS.Packet.AdaptationField.OptionalFields2 as OptionalFields2

data Flags3 = MkFlags5 Bool Bool Bool

class Class a where
  pcr                               :: a -> Word64 -- 42
  opcr                              :: a -> Word64 -- 42
  splice_countdown                  :: a -> Word8  -- 8
  transport_private_data_length     :: a -> Word8 -- 8
  transport_private_data            :: a -> ByteString -- depends on transport_private_data_length
  adaptation_field_extension_length :: a -> Word8 -- 8
  flags3                            :: a -> Flags3 -- 3
  optional_fields_2                 :: a -> OptionalFields2.Data

data Data = MkData {
  _pcr                               :: Word64, -- 42
  _opcr                              :: Word64, -- 42
  _splice_countdown                  :: Word8,  -- 8
  _transport_private_data_length     :: Word8, -- 8
  _transport_private_data            :: ByteString, -- depends on transport_private_data_length
  _adaptation_field_extension_length :: Word8, -- 8
  _flags3                            :: Flags3, -- 3
  _optional_fields_2                 :: OptionalFields2.Data
  }
