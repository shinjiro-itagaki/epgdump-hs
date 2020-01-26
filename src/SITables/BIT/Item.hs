{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.BIT.Item(
  Data,
  Class(..),
  ) where

import qualified BytesReader.Base as BytesReaderBase
import qualified Parser.Result as Result
import qualified Descriptor
import Data.Vector(Vector,toList,empty)
import qualified Utils.FromByteString as FromByteString
import qualified Data.ByteString.Lazy as BS
import qualified Utils.EmptyExist as EmptyExist
import Utils

class Class a where
  broadcaster_id                 :: a -> Word8
  reserved_future_use            :: a -> Word8
  broadcaster_descriptors_length :: a -> Word16
  descriptors                    :: a -> [Descriptor.Data]
  
data Data = MkData {
  _broadcaster_id                 :: Word8,
  _reserved_future_use            :: Word8,
  _broadcaster_descriptors_length :: Word16,
  _descriptors                    :: Vector Descriptor.Data
  } deriving (Show)

instance Class Data where
  broadcaster_id                 = _broadcaster_id
  reserved_future_use            = _reserved_future_use
  broadcaster_descriptors_length = _broadcaster_descriptors_length

instance EmptyExist.Class Data where
  mkEmpty = MkData {
  _broadcaster_id                 = mkEmpty,
  _reserved_future_use            = mkEmpty,
  _broadcaster_descriptors_length = mkEmpty,
  _descriptors                    = Data.Vector.empty
  }

instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    let (broadcaster_id,bs0)           = fromByteStringWithRest bs
        (w16,bs1)                      = fromByteStringWithRest bs0
        reserved_future_use            = toWord8 $ w16 .&. 0xF000 --4
        broadcaster_descriptors_length = w16 .&. 0x0FFF --12
        (bs2,rest)                     = BS.splitAt (fromInteger $ toInteger broadcaster_descriptors_length) bs1
        descriptors                    = fromByteString bs2
        d = MkData {
          _broadcaster_id = broadcaster_id,
          _reserved_future_use = reserved_future_use,
          _broadcaster_descriptors_length = broadcaster_descriptors_length,
          _descriptors = descriptors
          }
    in (d, rest)
