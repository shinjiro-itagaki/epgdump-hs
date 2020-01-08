module Descriptor.VideoDecodeControl (
  Class(..)
  ,Data
  ) where
import Descriptor.Common(Base(..),Descriptor(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => Class a where
  still_picture_flag :: a -> Bool
  sequence_end_code_flag :: a -> Bool
  video_encode_format :: a -> Word8
--  reserved_future_use :: a -> Word8 -- 2

data Data = MkData {
  _descriptor_tag         :: Word8,
  _descriptor_length      :: Word8,
  _still_picture_flag     :: Bool,
  _sequence_end_code_flag :: Bool,
  _video_encode_format    :: Word8
  }

instance Descriptor Data where
  descriptor_tag    = _descriptor_tag
  descriptor_length = _descriptor_length

instance Base Data where
  fromByteString bs = (Nothing, bs)

instance Class Data where
  still_picture_flag     = _still_picture_flag
  sequence_end_code_flag = _sequence_end_code_flag
  video_encode_format    = _video_encode_format
