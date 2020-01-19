{-# LANGUAGE FlexibleInstances #-}
module TS.Packet.AdaptationField where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Bits(Bits(..))
import Common(ByteString)
import qualified TS.Packet.AdaptationField.OptionalFields as OptionalFields
import qualified Data.ByteString.Lazy as BS
import qualified Parser.Result as Result
import Parser.Result((>>==))

type Flags5 = (Bool,Bool,Bool,Bool,Bool)

class Class a where
  adaptation_field_length              :: (Num b) => a -> b -- 8
  discontinuity_indicator              :: a -> Bool -- 1
  random_access_indicator              :: a -> Bool -- 1
  elementary_stream_priority_indicator :: a -> Bool -- 1
  flags5                               :: a -> Flags5 -- 5
  optional_fields                      :: a -> OptionalFields.Data 
  stuffing_bytes                       :: a -> ByteString -- ?? (rest bytes)
  parse                                :: [Word8] -> (Result.Data a,[Word8])

data Data = MkData {
  _adaptation_field_length              :: Word8, -- 8
  _discontinuity_indicator              :: Bool, -- 1
  _random_access_indicator              :: Bool, -- 1
  _elementary_stream_priority_indicator :: Bool, -- 1
  _flags5                               :: Flags5, -- 5
  _optional_fields                      :: OptionalFields.Data,
  _stuffing_bytes                       :: ByteString -- ??
  }

mkEmpty = MkData {
  _adaptation_field_length              = 0,
  _discontinuity_indicator              = False,
  _random_access_indicator              = False,
  _elementary_stream_priority_indicator = False,
  _flags5                               = (False,False,False,False,False),
  _optional_fields                      = OptionalFields.mkEmpty,
  _stuffing_bytes                       = BS.empty
  }

    
flow1 :: Data -> [Word8] -> (Result.Data Data, [Word8])
flow1 d (x:xs)
  | toInteger (Prelude.length xs) < (toInteger x) = (Result.DataIsTooShort $ Just $ fromInteger $ toInteger $ ((x -) $ fromInteger $ toInteger $ Prelude.length xs), [])
  | otherwise = (Result.Parsed (d {_adaptation_field_length = x}), Prelude.drop (fromInteger $ toInteger x) xs)

instance Class Data where
  adaptation_field_length              = fromInteger . toInteger . _adaptation_field_length
  discontinuity_indicator              = _discontinuity_indicator
  random_access_indicator              = _random_access_indicator
  elementary_stream_priority_indicator = _elementary_stream_priority_indicator
  flags5                               = _flags5
  optional_fields                      = _optional_fields
  stuffing_bytes                       = _stuffing_bytes
  parse (x:xs) =
    (Result.Parsed mkEmpty,xs)
    >>== flow1
