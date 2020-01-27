{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SITables.SDT.Item(
  Data,
  Class(..),
  ) where

import qualified BytesReader.Base as BytesReaderBase
import qualified Descriptor
import Data.Vector(Vector,toList,empty)
import qualified Parser.Result as Result
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.FromByteString as FromByteString
import qualified Data.ByteString.Lazy as BS
import qualified Descriptor

import Utils

class Class a where
  service_id                 :: a -> Word16
  reserved_future_use        :: a -> Word8
  eit_user_defined_flags     :: a -> (Bool,Bool,Bool)
  eit_schedule_flag          :: a -> Bool
  eit_present_following_flag :: a -> Bool
  running_status             :: a -> Word8
  free_CA_mode               :: a -> Bool
  descriptors_loop_length    :: a -> Word16
  descriptors                :: a -> [Descriptor.Data]

data Data = MkData {
  _service_id                 :: Word16,
  _reserved_future_use        :: Word8,
  _eit_user_defined_flags     :: (Bool,Bool,Bool),
  _eit_schedule_flag          :: Bool,
  _eit_present_following_flag :: Bool,
  _running_status             :: Word8,
  _free_CA_mode               :: Bool,
  _descriptors_loop_length    :: Word16,
  _descriptors                :: Vector Descriptor.Data
  } deriving (Show)

instance Class Data where
  service_id                 = _service_id  
  reserved_future_use        = _reserved_future_use
  eit_user_defined_flags     = _eit_user_defined_flags
  eit_schedule_flag          = _eit_schedule_flag
  eit_present_following_flag = _eit_present_following_flag
  running_status             = _running_status
  free_CA_mode               = _free_CA_mode
  descriptors_loop_length    = _descriptors_loop_length
  descriptors                = toList . _descriptors

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty Data.Vector.empty

instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    let (service_id,bs0)           = fromByteStringWithRest bs
        ((w8,w16),bs1)             = fromByteStringWithRest bs0
        reserved_future_use        =             (w8 .&. 0xE0) `shiftR` 5 --3
        eit_user_defined_flags     = fromWord8 $ (w8 .&. 0x1C) `shiftR` 2 --3
        eit_schedule_flag          = fromWord8 $ (w8 .&. 0x02) `shiftR` 1 --1
        eit_present_following_flag = fromWord8 $ (w8 .&. 0x01) `shiftR` 0 --1
        running_status             = fromWord16 $ (w16 .&. 0xE000) `shiftR` 13 --3
        free_CA_mode               = fromWord16 $ (w16 .&. 0x1000) `shiftR` 12  -- 1
        descriptors_loop_length    =              (w16 .&. 0x0FFF) -- 12

        (bs2, rest) = BS.splitAt (fromInteger $ toInteger descriptors_loop_length) bs1
        descriptors = fromByteString bs2

        d = MkData {
          _service_id                 = service_id                 ,
          _reserved_future_use        = reserved_future_use        ,
          _eit_user_defined_flags     = eit_user_defined_flags     ,
          _eit_schedule_flag          = eit_schedule_flag          ,
          _eit_present_following_flag = eit_present_following_flag ,
          _running_status             = running_status             ,
          _free_CA_mode               = free_CA_mode               ,
          _descriptors_loop_length    = descriptors_loop_length    ,
          _descriptors                = descriptors
          }
    in (d, rest)
