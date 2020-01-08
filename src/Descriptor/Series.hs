module Descriptor.Series where
import Descriptor.Common(Base,AreaCode)
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

  
class Base a => Series a where
  series_id :: a -> Word16
  repeat_label :: a -> Word8
  program_pattern :: a -> Word8
  expire_date_valid_flag :: a -> Bool
  expire_date :: a -> Word16
  episode_number :: a -> Word16
  last_episode_number :: a -> Word16
  series_name :: a -> String
