module Descriptor.VideoDecodeControl where
import Descriptor.Common(Base)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Base a => VideoDecodeControl a where
  still_picture_flag :: a -> Bool
  sequence_end_code_flag :: a -> Bool
  video_encode_format :: a -> Word8
--  reserved_future_use :: a -> Word8 -- 2

