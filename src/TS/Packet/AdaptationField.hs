{-# LANGUAGE FlexibleInstances #-}
module TS.Packet.AdaptationField where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Bits(Bits(..))
import Common(ByteString)
import qualified TS.Packet.Header as Header
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
  parse                                :: Header.Data -> ByteString -> (Result.Data (Maybe a),ByteString)

data Data = MkData {
  _adaptation_field_length              :: Word8, -- 8
  _discontinuity_indicator              :: Bool, -- 1
  _random_access_indicator              :: Bool, -- 1
  _elementary_stream_priority_indicator :: Bool, -- 1
  _flags5                               :: Flags5, -- 5
  _optional_fields                      :: OptionalFields.Data,
  _stuffing_bytes                       :: ByteString -- ??
  } deriving(Eq,Show)

mkEmpty = MkData {
  _adaptation_field_length              = 0,
  _discontinuity_indicator              = False,
  _random_access_indicator              = False,
  _elementary_stream_priority_indicator = False,
  _flags5                               = (False,False,False,False,False),
  _optional_fields                      = OptionalFields.mkEmpty,
  _stuffing_bytes                       = BS.empty
  }

-- TODO
-- AdaptationFieldの詳細な内容についてはパースしていないので後日、必要があれば実装する
flow1 :: Data -> ByteString -> (Result.Data (Maybe Data), ByteString)
flow1 d bytes =
  case  BS.uncons bytes of
    Nothing -> (Result.DataIsTooShort Nothing, bytes)
    Just (len,bytes') ->
      let byteslen' = toInteger $ BS.length bytes'
          len'      = toInteger len
          len''     = fromInteger len'
      in if byteslen' < len'
         then (Result.DataIsTooShort $ Just $ fromInteger $ len' - byteslen'            , BS.empty)
         else (Result.Parsed         $ Just $ (d {_adaptation_field_length = fromInteger len'}) , BS.drop len'' bytes')

instance Class Data where
  adaptation_field_length              = fromInteger . toInteger . _adaptation_field_length
  discontinuity_indicator              = _discontinuity_indicator
  random_access_indicator              = _random_access_indicator
  elementary_stream_priority_indicator = _elementary_stream_priority_indicator
  flags5                               = _flags5
  optional_fields                      = _optional_fields
  stuffing_bytes                       = _stuffing_bytes
  parse h bytes =
    if Header.has_adaptation_field h then
      flow1 mkEmpty bytes
--      (\(a,b) -> ((Result.map Just a),b)) $ ((Result.Parsed $ mkEmpty,bytes)
--      >>== flow1)
    else
      (Result.Parsed Nothing, bytes)
