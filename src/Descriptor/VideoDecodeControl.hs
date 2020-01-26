module Descriptor.VideoDecodeControl (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header

class (Base.Class a) => Class a where
  still_picture_flag :: a -> Bool
  sequence_end_code_flag :: a -> Bool
  video_encode_format :: a -> Word8
  reserved_future_use :: a -> Word8 -- 2

data Data = MkData {
  _header                 :: Header.Data,
  _still_picture_flag     :: Bool,
  _sequence_end_code_flag :: Bool,
  _video_encode_format    :: Word8,
  _reserved_future_use    :: Word8 
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where

instance Class Data where
  still_picture_flag     = _still_picture_flag
  sequence_end_code_flag = _sequence_end_code_flag
  video_encode_format    = _video_encode_format
  reserved_future_use    = _reserved_future_use
