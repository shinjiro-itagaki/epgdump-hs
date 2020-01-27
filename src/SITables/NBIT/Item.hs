{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.NBIT.Item(
  Data,
  Class(..),
  ) where

import qualified BytesReader.Base as BytesReaderBase
import qualified Parser.Result as Result
import qualified Descriptor
import Data.Vector(Vector,toList,empty,snoc)
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.FromByteString as FromByteString
import qualified Data.ByteString.Lazy as BS
import Utils

class Class a where
  information_id            :: a -> Word16
  information_type          :: a -> Word8
  description_body_location :: a -> (Bool,Bool)
  reserved_future_use1      :: a -> Word8
  user_defined              :: a -> Word8
  number_of_keys            :: a -> Word8
  keys                      :: a -> [Word16]
  reserved_future_use2      :: a -> Word8
  descriptors_loop_length   :: a -> Word16
  descriptors               :: a -> [Descriptor.Data]
  
data Data = MkData {
  _information_id            :: Word16,
  _information_type          :: Word8,
  _description_body_location :: (Bool,Bool),
  _reserved_future_use1      :: Word8, -- 2,
  _user_defined              :: Word8,
  _number_of_keys            :: Word8,
  _keys                      :: Vector Word16,
  _reserved_future_use2      :: Word8, -- 4
  _descriptors_loop_length   :: Word16,
  _descriptors               :: Vector Descriptor.Data
  } deriving (Show)

instance Class Data where
  information_id            = _information_id
  information_type          = _information_type 
  description_body_location = _description_body_location
  reserved_future_use1      = _reserved_future_use1 
  user_defined              = _user_defined
  number_of_keys            = _number_of_keys
  keys                      = toList . _keys
  reserved_future_use2      = _reserved_future_use2
  descriptors_loop_length   = _descriptors_loop_length
  descriptors               = toList . _descriptors

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty Data.Vector.empty mkEmpty mkEmpty Data.Vector.empty

instance FromByteString.Class Data where
  fromByteStringWithRest bs = 
    let ((information_id,
          w8,
          user_defined,
          number_of_keys
         ),bs0) = fromByteStringWithRest bs
        information_type          =             (w8 .&. 0xF0) `shiftR` 4
        description_body_location = fromWord8 $ (w8 .&. 0x0C) `shiftR` 2
        reserved_future_use1      =             (w8 .&. 0x03)
        (bs1,bs2) = BS.splitAt ((* 2) $ fromInteger $ toInteger number_of_keys) bs0
        keys = fromByteString bs1
        (w16,bs3) = fromByteStringWithRest bs2
        reserved_future_use2    = toWord8 $ (w16 .&. 0xF000) `shiftR` 12
        descriptors_loop_length =           (w16 .&. 0x0FFF) `shiftR` 0
        (bs4,rest) = BS.splitAt (fromInteger $ toInteger descriptors_loop_length) bs3
        descriptors = fromByteString bs4
        d = MkData {
          _information_id            = information_id,
          _information_type          = information_type,
          _description_body_location = description_body_location,
          _reserved_future_use1      = reserved_future_use1,
          _user_defined              = user_defined,
          _number_of_keys            = number_of_keys,
          _keys                      = keys,
          _reserved_future_use2      = reserved_future_use2,
          _descriptors_loop_length   = descriptors_loop_length,
          _descriptors               = descriptors
          }
    in (d,rest)
