{-# LANGUAGE FlexibleInstances #-}

module Descriptor.Common where
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class Descriptor a where
  descriptor_tag    :: a -> Word8
  descriptor_length :: a -> Word8

class (Descriptor a) => Base a where
  fromByteString :: ByteString -> (a,ByteString)

class HasName a where
  name :: a -> String

--class HasReservedFutureUse a  where
--  reserved_future_use :: a -> Word8


class HasISO_639_LanguageCode a where
  iso_639_language_code :: a -> LangCode

class HasText a where
  text :: a -> String

class HasText a => HasTextAndLen a where
  text_length :: a -> Word8

class HasTitle a where
  title :: a -> String
  title_length :: a -> Word8

class HasServiceID a where
  service_id :: a -> Word16
  
class HasServiceType a where
  service_type :: a -> Word8


class HasOriginalNetworkID a where
  original_network_id :: a -> Word16

class (HasServiceID a, HasOriginalNetworkID a) => TOS a where
  transport_stream_id :: a -> Word16
--  original_network_id :: a -> Word16
--  service_id          :: a -> Word16

class HasPrivateDataBytes a where
  private_data_bytes :: a -> [Word8]


class HasReferenceServiceID a where
  reference_service_id :: a -> Word16

class HasEventID a where
  event_id :: a -> Word16
  
class HasComponentTag a where
  component_tag :: a -> Word8

class (HasComponentTag a) => HasComponent a where
  stream_content :: a -> Word8
  component_type :: a -> Word8

class HasCountryCode a where
  country_code :: a -> CountryCode

class HasUserDefined a where
  user_defined :: a -> Word8

class HasDataComponentID a where
  data_component_id :: a -> Word16

class HasModuleID a where
  module_id :: a -> Word16

class HasContentID a where
  content_id :: a -> Word32

class HasSelector a where
  selector_length   :: a -> Word8
  selector_bytes    :: a -> [Word8]  

class HasMaybePrivateDataBytes a where
  maybe_private_data_bytes :: a -> Maybe [Word8]

type LangCode = (Char,Char,Char)

type CountryCode = (Char,Char,Char)

data TOSData = MkTOSData {
  _original_network_id :: Word16,
  _transport_stream_id :: Word16,
  __service_id :: Word16
    }

type AreaCode = Word16

instance HasOriginalNetworkID TOSData where
  original_network_id = _original_network_id

instance TOS TOSData where
  transport_stream_id = _transport_stream_id
  
instance HasServiceID TOSData where  
  service_id = __service_id
