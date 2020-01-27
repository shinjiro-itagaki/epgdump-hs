{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SITables.PCAT.Item(
  Data,
  Class(..),
  ) where

import qualified Utils.Schedule as Schedule
import qualified BytesReader.Base as BytesReaderBase
import qualified Parser.Result as Result
import Utils
import qualified Descriptor
import Data.Vector(Vector,toList,empty)
import qualified Utils.EmptyExist as EmptyExist
import qualified Utils.FromByteString as FromByteString
import qualified Data.ByteString.Lazy as BS

class Class a where
  content_version             :: a -> Word16
  content_minor_version       :: a -> Word16
  version_indicator           :: a -> (Bool,Bool)
  reserved_future_use1        :: a -> (Bool,Bool)
  content_descriptor_length   :: a -> Word16
  reserved_future_use2        :: a -> Word8
  schedule_description_length :: a -> Word16
  schedules                   :: a -> [Schedule.Data]
  descriptors                 :: a -> [Descriptor.Data]

  
data Data = MkData {
  _content_version             :: Word16,
  _content_minor_version       :: Word16,
  _version_indicator           :: (Bool,Bool),
  _reserved_future_use1        :: (Bool,Bool),
  _content_descriptor_length   :: Word16,
  _reserved_future_use2        :: Word8,
  _schedule_description_length :: Word16,
  _schedules                   :: Vector Schedule.Data,
  _descriptors                 :: Vector Descriptor.Data
  } deriving (Show)

instance Class Data where
  content_version             = _content_version
  content_minor_version       = _content_minor_version
  version_indicator           = _version_indicator
  reserved_future_use1        = _reserved_future_use1
  content_descriptor_length   = _content_descriptor_length
  reserved_future_use2        = _reserved_future_use2
  schedule_description_length = _schedule_description_length
  schedules                   = toList . _schedules
  descriptors                 = toList . _descriptors

instance EmptyExist.Class Data where
  mkEmpty = MkData mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty mkEmpty Data.Vector.empty Data.Vector.empty

instance FromByteString.Class Data where
  fromByteStringWithRest bs =
    let ((content_version, content_minor_version, w16_0, w16_1),bs0) = fromByteStringWithRest bs
        version_indicator           = fromWord16 $ (w16_0 .&. 0xC000) `shiftR` 14 -- 2
        reserved_future_use1        = fromWord16 $ (w16_0 .&. 0x3000) `shiftR` 12 -- 2
        content_descriptor_length   =              (w16_0 .&. 0x0FFF) -- 12
        reserved_future_use2        = fromWord16 $ (w16_1 .&. 0xF000) `shiftR` 12 -- 4
        schedule_description_length =              (w16_1 .&. 0x0FFF) -- 12
        (bs1,bs2) = BS.splitAt (fromInteger $ toInteger schedule_description_length) bs0
        schedules = fromByteString bs1
        (bs3,rest) = BS.splitAt (fromInteger $ toInteger content_descriptor_length) bs2
        descriptors = fromByteString bs3
        d = MkData {
          _content_version             = content_version,
          _content_minor_version       = content_minor_version,
          _version_indicator           = version_indicator, 
          _reserved_future_use1        = reserved_future_use1, 
          _content_descriptor_length   = content_descriptor_length,
          _reserved_future_use2        = reserved_future_use2,
          _schedule_description_length = schedule_description_length,
          _schedules                   = schedules,
          _descriptors                 = descriptors
          }
    in (d,rest)

