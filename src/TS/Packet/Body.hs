{-# LANGUAGE FlexibleInstances #-}
module TS.Packet.Body where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Bits(Bits(..))
import Data.ByteString.Lazy(ByteString,length,empty)
import qualified TS.Packet.Header as Header
import qualified TS.Packet.AdaptationField as AdaptationField
import Common(EmptyExist(..))

class (EmptyExist a) => Class a where
  body             :: a -> Data
  adaptation_field :: (Header.Class header) => a -> header -> Maybe AdaptationField.Data
  payload          :: (Header.Class header) => a -> header -> Header.Data -> ByteString
  payloadlen       :: (Header.Class header) => a -> header -> Word8
  
  adaptation_field = adaptation_field . body
  payload          = payload          . body
  payloadlen       = payloadlen       . body

type Data = ByteString

--instance EmptyExist Data where
--  mkEmpty = empty

instance Class Data where
  body x = x
  -- hoge
  adaptation_field x header = if Header.has_adaptation_field header then Nothing else Nothing
  payloadlen       x header = if Header.has_payload header then selflen - adlen else 0
    where
      selflen = fromInteger $ toInteger $ Data.ByteString.Lazy.length x -- 自身の長さ
      adlen = case adaptation_field x header of
                Just af -> AdaptationField.adaptation_field_length af
                Nothing -> 0

parse :: ByteString -> Data
parse x = x
