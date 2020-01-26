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

-- _parseIOFlow1 :: (BytesReaderBase.Class bh) => bh -> Data -> IO (Result.Data Data, bh)
-- _parseIOFlow1 fh init = do
--   getBitsIO_M fh [
--     (16, (\(v,d) -> d { _description_id          = fromWord64 v})),
--     (12, (\(v,d) -> d { _reserved_future_use     = fromWord64 v})),
--     (12, (\(v,d) -> d { _descriptors_loop_length = fromWord64 v}))
--     ] init
