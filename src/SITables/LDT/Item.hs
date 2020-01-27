{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.LDT.Item(
  Data,
  Class(..),
  ) where

import qualified BytesReader.Base as BytesReaderBase
import qualified Parser.Result as Result
import Utils
import qualified Descriptor
import qualified Utils.EmptyExist as EmptyExist
import Data.Vector(Vector,toList,empty)
import qualified Utils.FromByteString as FromByteString
import qualified Data.ByteString.Lazy as BS

class Class a where
  description_id          :: a -> Word16
  descriptors_loop_length :: a -> Word16
  reserved_future_use     :: a -> Word16

data Data = MkData {
  _description_id          :: Word16,
  _reserved_future_use     :: Word16, -- 12
  _descriptors_loop_length :: Word16,
  _descriptors             :: Vector Descriptor.Data
  } deriving (Show)

instance Class Data where
  description_id          = _description_id 
  descriptors_loop_length = _descriptors_loop_length
  reserved_future_use     = _reserved_future_use
  
instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty Data.Vector.empty

instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    let ((description_id,
          reserved_future_use,
          descriptors_loop_length),bs0) = fromByteStringWithRest bs
        (bs1,rest) = BS.splitAt (fromInteger $ toInteger descriptors_loop_length) bs0
        descriptors = fromByteString bs1
        d = MkData {
          _description_id          = description_id,
          _reserved_future_use     = reserved_future_use,
          _descriptors_loop_length = descriptors_loop_length,
          _descriptors             = descriptors
          }
    in (d,rest)
