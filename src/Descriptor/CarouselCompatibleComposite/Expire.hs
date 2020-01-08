module Descriptor.CarouselCompatibleComposite.Expire where
import Descriptor.CarouselCompatibleComposite.Common(SubDescriptor(..))
import Descriptor.Common(Base(..),Descriptor(..),HasText(..),HasName(..),HasText(..),HasISO_639_LanguageCode(..))
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (SubDescriptor a) => Expire a where
  time_mode :: a -> Word8
  mjd_jst_time :: a -> Maybe Word64
--  reserved_future_use
  passed_seconds :: a -> Maybe Word32

data ExpireData = MkExpireData {
  _expire_descriptor_tag    :: Word8,
  _expire_descriptor_length :: Word8,
  _expire_time_mode         :: Word8,
  _expire_mjd_jst_time      :: Maybe Word64,
  _expire_passed_seconds    :: Maybe Word32
  }

instance Descriptor ExpireData where
  descriptor_tag = _expire_descriptor_tag
  descriptor_length = _expire_descriptor_length

instance SubDescriptor ExpireData

instance Expire ExpireData where
  time_mode = _expire_time_mode
  mjd_jst_time = _expire_mjd_jst_time
  passed_seconds = _expire_passed_seconds

