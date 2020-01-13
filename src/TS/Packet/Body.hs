{-# LANGUAGE FlexibleInstances #-}
module TS.Packet.Body where

import Data.Word(Word64, Word32, Word16, Word8)
import Data.Bits(Bits(..))
import qualified Data.ByteString.Lazy as BS
-- import Data.ByteString.Lazy(ByteString,length,empty,take)
import qualified TS.Packet.Header as Header
import qualified TS.Packet.AdaptationField as AdaptationField
import Common(EmptyExist(..))

class (EmptyExist a) => Class a where
  -- please implement
  body_data        :: a -> BS.ByteString
  header           :: a -> Header.Data
  --

  -- not impl
  adaptation_field :: a -> Maybe AdaptationField.Data
  adaptation_field x = if Header.has_adaptation_field (header x) then Nothing else Nothing
  
  payload :: a -> BS.ByteString
  payload x = BS.drop (payloadlen x) $ body_data x
  
  payloadlen :: (Num b) => a -> b
  payloadlen x = if Header.has_payload header' then selflen - adlen else 0
    where
      header' =  header x
      selflen = fromInteger $ toInteger $ BS.length $ body_data x -- 自身の長さ
      adlen = case adaptation_field x of
                Just af -> AdaptationField.adaptation_field_length af
                Nothing -> 0

type Data = BS.ByteString

--instance EmptyExist Data where
--  mkEmpty = BS.empty

--instance Class Data where
--  body_data x = x
--  header = snd

--parse :: BS.ByteString -> Header.Data -> Data
--parse x header = (x,header)
