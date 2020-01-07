{-# LANGUAGE FlexibleInstances #-}

module Descriptor where

import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS

class Base a where
  descriptor_tag    :: a -> Word8
  descriptor_length :: a -> Word8
  fromByteString    :: ByteString -> (a,ByteString)

class HasName a where
  name :: a -> String

--class HasReservedFutureUse a  where
--  reserved_future_use :: a -> Word8

type LangCode = (Char,Char,Char)

class HasISO_639_LanguageCode a where
  iso_639_language_code :: a -> LangCode

class HasText a where
  text :: a -> String

class HasText a => HasTextAndLen a where
  text_length :: a -> Word8

class Base a => ConditionalAccess a where
  
class Base a => Copyright a where
  
class (Base a, HasName a) => NetworkName a where
-- name :: String

class HasServiceID a where
  service_id :: a -> Word16
  
class HasServiceType a where
  service_type :: a -> Word8
  
data ServiceData = MkServiceData {
  _service_id :: Word16,
  _service_type :: Word8
  }

instance HasServiceID ServiceData where
  service_id = _service_id

instance HasServiceType ServiceData where
  service_type = _service_type
  
class Base a => ServiceList a where
  list :: a -> [ServiceData]

class Base a => Stuffing a where
  stuffing_bytes :: a -> [Word8]

class Base a => SatelliteDeliverySystem a where
  
class (Base a, HasName a) => BouquetName a where
  
class (Base a, HasServiceType a) => Service a where
--  service_type :: a -> Word8
  service_provider_name_length :: a -> Word8
  service_provider_name :: a -> String
  service_name_length :: a -> Word8
  service_name :: a -> String

type CountryCode = (Char,Char,Char)
  
class (Base a) => CountryAvailability a where
  country_available_flag :: a -> Bool
--  reserved_future_use :: a -> Word8
  country_codes :: a -> [CountryCode]

class (HasServiceID a) => TOS a where
  transport_stream_id :: a -> Word16
  original_network_id :: a -> Word16
--  service_id          :: a -> Word16
  
class (Base a, TOS a) => Linkage a where
--  transport_stream_id :: a -> Word16
--  original_network_id :: a -> Word16
--  service_id          :: a -> Word16
  linkage_type        :: a -> Word8
  private_data_bytes  :: a -> [Word8]

class Base a => NVOD_Reference a where
  references :: a -> [TOSData]
  

class HasReferenceServiceID a where
  reference_service_id :: a -> Word16
  
class (Base a, HasReferenceServiceID a) => TimeShiftedService a where
--  reference_service_id :: a -> Word16
  
class (Base a, HasISO_639_LanguageCode a, HasTextAndLen a) => ShortEvent a where
-- iso_639_language_code :: a -> LangCode
  event_name_length :: a -> Word8
  event_name :: a -> String
--  text_length :: a -> Word8
--  text :: a -> String

data ExtendedEventItem = MkExtendedEventItem {
  item_description_length :: Word8,
  item_description_chars :: String,
  item_length :: Word8,
  item_chars :: String
  }
                         
class (Base a, HasISO_639_LanguageCode a, HasTextAndLen a) => ExtendedEvent a where
  descriptor_number :: a -> Word8
  last_descriptor_number :: a -> Word8
--  iso_639_language_code :: a -> LangCode
  length_of_items :: a -> Word8
  extended_event_items :: a -> [ExtendedEventItem]
--  text_length :: a -> Word8
--  text :: a -> String
  
class (Base a,HasReferenceServiceID a) => TimeShiftedEvent a where
--  reference_service_id :: a -> Word16
  reference_event_id :: a -> Word16
  
class Base a => Component a where

data ElementaryCellField = MkElementaryCellField {
--  reserved_future_use
  elementary_cell_id :: Word8
    }

data TOSData = MkTOSData {
  _original_network_id :: Word16,
  _transport_stream_id :: Word16,
  __service_id :: Word16
    }

instance TOS TOSData where
  original_network_id = _original_network_id
  transport_stream_id = _transport_stream_id
  
instance HasServiceID TOSData where  
  service_id = __service_id

-- class TOS a => MosaicItem a where
data MosaicItem = MkMosaicItem {
  logical_cell_id :: Word8,
--  reserved_future_use,
  logical_cell_presentation_info :: Word8,
  elementary_cell_field_length :: Word8,
  elementary_cell_fields :: [ElementaryCellField],
  cell_linkage_info :: Word8,
  bouquet_id :: Maybe Word16,
--  original_network_id :: Word16,
--  transport_stream_id :: Word16,
--  service_id :: Word16,
  _mosaic_item_event_id :: Word16,
  tosdata :: TOSData -- 非公開
  }

class HasEventID a where
  event_id :: a -> Word16

instance TOS MosaicItem where
  original_network_id = original_network_id . tosdata
  transport_stream_id = transport_stream_id . tosdata
  
instance HasServiceID MosaicItem where  
  service_id = service_id . tosdata

instance HasEventID MosaicItem where
  event_id = _mosaic_item_event_id
  
class (Base a) => Mosaic a where
  mosaic_entry_point :: a -> Bool
  number_of_horizontal_elementary_cells :: a -> Word8
-- reserved_future_use :: a -> Word8
  number_of_vertical_elementary_cells :: a -> Word8
  mosaic_items :: a -> [MosaicItem]
  
class HasComponentTag a where
  component_tag :: a -> Word8

class (HasComponentTag a) => HasComponent a where
  stream_content :: a -> Word8
  component_type :: a -> Word8
  
class (Base a, HasComponentTag a) => StreamIdentifier a where
--  component_tag :: a -> Word8
  
class Base a => CAIdentifier a where
  ca_system_id :: a -> Word16

data ContentBody = MkContentBody {
  content_nibble_level_1 :: Word8,
  content_nibble_level_2 :: Word8,
  user_nibble_1 :: Word8,
  user_nibble_2 :: Word8
  }

class Base a => Content a where
  body :: a -> [ContentBody]

class HasCountryCode a where
  country_code :: a -> CountryCode

data ParentalRatingItem = MkParentalRatingItemData { 
  _country_code :: CountryCode,
  rating :: Word8
  }

instance HasCountryCode ParentalRatingItem where
  country_code = _country_code
  
class Base a => ParentalRating a where
  ratings :: a -> [ParentalRatingItem]
  
class (Base a) => HierarchicalTransmission a where
  quality_level :: a -> Bool
  reference_pid :: a -> Word16
  
class ComponentControl a where
  digital_recording_control_data :: a -> Word8
  maximum_bitrate_flag           :: a -> Bool
--  reserved_future_use            :: a -> Bool
  user_defined                   :: a -> Word8
  maximum_bitrate                :: a -> Maybe Word8

data ComponentControlData = MkComponentControl {
  _component_tag                  :: Word8,
  _digital_recording_control_data :: Word8,
  _maximum_bitrate_flag           :: Bool,
  _user_defined                   :: Word8,
  _maximum_bitrate                :: Maybe Word8  
  }

instance ComponentControl ComponentControlData where
  digital_recording_control_data = _digital_recording_control_data
  maximum_bitrate_flag           = _maximum_bitrate_flag
  user_defined                   = _user_defined
  maximum_bitrate                = _maximum_bitrate

instance HasComponentTag ComponentControlData where
  component_tag = _component_tag


class (Base a, ComponentControl a) => DigitalCopyControl a where
--  digital_recording_control_data :: a -> Word8
--  maximum_bitrate_flag           :: a -> Bool
  component_control_flag         :: a -> Bool
--  user_defined                   :: a -> Word8
--  maximum_bitrate                :: a -> Maybe Word8
  component_control_length       :: a -> Maybe Word8
  component_controls             :: a -> Maybe [ComponentControlData]

type AreaCode = Word16
  
class (HasServiceID a) => EmergencyInformationItem a where
--  service_id :: a -> Word16
  start_end_flag :: a -> Bool
  signal_level :: a -> Bool
-- reserved_future_use :: a -> Word8
  area_code_length :: a -> Word8
  area_codes :: a -> [AreaCode]
  
class Base a => EmergencyInformation a where

class HasDataComponentID a where
  data_component_id :: a -> Word16
  
class (Base a, HasDataComponentID a) => DataComponent a where
--  data_component_id :: a -> Word16
  additional_data_component_info :: a -> [Word8]
  
class Base a => SystemControl a where

class HasCountryCode a => LocalTimeOffsetItem a where
--  country_code               :: a -> CountryCode
  country_region_id          :: a -> Word8
  reserved                   :: a -> Bool
  local_time_offset_polarity :: a -> Bool
  local_time_offset          :: a -> Word16
  time_of_change             :: a -> Word64
  next_time_offset           :: a -> Word16

data LocalTimeOffsetItemData = MkLocalTimeOffsetItemData {
  _local_time_offest_item_data_country_code :: CountryCode,
  _country_region_id          :: Word8,
  _reserved                   :: Bool,
  _local_time_offset_polarity :: Bool,
  _local_time_offset          :: Word16,
  _time_of_change             :: Word64,
  _next_time_offset           :: Word16
  }


instance HasCountryCode LocalTimeOffsetItemData where
  country_code = _local_time_offest_item_data_country_code
  
instance LocalTimeOffsetItem LocalTimeOffsetItemData where
  country_region_id          = _country_region_id
  reserved                   = _reserved
  local_time_offset_polarity = _local_time_offset_polarity
  local_time_offset          = _local_time_offset
  time_of_change             = _time_of_change
  next_time_offset           = _next_time_offset
  
class Base a => LocalTimeOffset a where
  items :: a -> [LocalTimeOffsetItemData]
  
class (Base a, HasComponent a, HasISO_639_LanguageCode a, HasText a) => AudioComponent a where
-- reserved_future_use :: a -> Word8
--  stream_content :: a -> Word8
--  component_type :: a -> Word8
--  component_tag  :: a -> Word8
  stream_type    :: a -> Word8
  simulcast_group_tag :: a -> Word8
  es_multi_lingual_flag :: a -> Bool
  main_component_flag :: a -> Bool
  quality_indicator :: a -> Word8
  sampling_rate :: a -> Word8
-- reserved_future_use :: a -> Word8
--  iso_639_language_code :: a -> LangCode
  iso_639_language_code2 :: a -> Maybe LangCode
--  text :: a -> String

data LinkServiceInfo = MkLinkServiceInfo {
  _link_service_info_original_network_id :: Word16,
  _link_service_info_transport_stream_id :: Word16,
  _link_service_info_service_id :: Word16
  } -- 0x01

instance TOS LinkServiceInfo where
  original_network_id = _link_service_info_original_network_id
  transport_stream_id = _link_service_info_transport_stream_id
  
instance HasServiceID LinkServiceInfo where  
  service_id = _link_service_info_service_id
  
data LinkEventInfo = MkLinkEventInfo {
  _link_event_info_original_network_id :: Word16,
  _link_event_info_transport_stream_id :: Word16,
  _link_event_info_service_id :: Word16,
  _link_event_info_event_id :: Word16
  } -- 0x02

instance TOS LinkEventInfo where
  original_network_id = _link_event_info_original_network_id
  transport_stream_id = _link_event_info_transport_stream_id
  
instance HasServiceID LinkEventInfo where  
  service_id = _link_event_info_service_id

instance HasEventID LinkEventInfo where
  event_id = _link_event_info_event_id

class HasModuleID a where
  module_id :: a -> Word16

class HasContentID a where
  content_id :: a -> Word32
  
data LinkModuleInfo  = MkLinkModuleInfo {
  _link_module_info_original_network_id :: Word16,
  _link_module_info_transport_stream_id :: Word16,
  _link_module_info_service_id :: Word16,
  _link_module_info_event_id :: Word16,
  _link_module_info_component_tag :: Word8,
  _link_module_info_module_id :: Word16
    } -- 0x03

instance TOS LinkModuleInfo where
  original_network_id = _link_module_info_original_network_id
  transport_stream_id = _link_module_info_transport_stream_id
  
instance HasServiceID LinkModuleInfo where  
  service_id = _link_module_info_service_id

instance HasEventID LinkModuleInfo where
  event_id = _link_module_info_event_id

instance HasComponentTag LinkModuleInfo where
  component_tag = _link_module_info_component_tag
  
instance HasModuleID LinkModuleInfo where
  module_id = _link_module_info_module_id

data LinkContentInfo = MkLinkContentInfo {
  _link_content_info_original_network_id :: Word16,
  _link_content_info_transport_stream_id :: Word16,
  _link_content_info_service_id :: Word16,
  _link_content_info_content_id :: Word32
  } -- 0x04

instance TOS LinkContentInfo where
  original_network_id = _link_content_info_original_network_id
  transport_stream_id = _link_content_info_transport_stream_id
  
instance HasServiceID LinkContentInfo where  
  service_id = _link_content_info_service_id

instance HasContentID LinkContentInfo where
  content_id = _link_content_info_content_id

data LinkContentModuleInfo = LinkContentModuleInfo {
  _link_content_module_original_network_id :: Word16,
  _link_content_module_transport_stream_id :: Word16,
  _link_content_module_service_id :: Word16,
  _link_content_module_content_id :: Word32,
  _link_content_module_component_tag :: Word8,
  _link_content_module_module_id :: Word16
  } -- 0x05

instance TOS LinkContentModuleInfo where
  original_network_id = _link_content_module_original_network_id
  transport_stream_id = _link_content_module_transport_stream_id
  
instance HasServiceID LinkContentModuleInfo where  
  service_id = _link_content_module_service_id

instance HasContentID LinkContentModuleInfo where
  content_id = _link_content_module_content_id

instance HasComponentTag LinkContentModuleInfo where
  component_tag = _link_content_module_component_tag
  
instance HasModuleID LinkContentModuleInfo where
  module_id = _link_content_module_module_id

data LinkErtNodeInfo = MkLinkErtNodeInfo {
  information_provider_id :: Word16,
  event_relation_id :: Word16
  } -- 0x06
  
data LinkStoredContentInfo = MkLinkStoredContentInfo {
  uri :: String
  } -- 0x07

data LinkDestination =
  MkService         LinkServiceInfo
  | MkEvent         LinkEventInfo
  | MkModule        LinkModuleInfo
  | MkContent       LinkContentInfo
  | MkContentModule LinkContentModuleInfo
  | MkErtNode       LinkErtNodeInfo
  | MkStoredContent LinkStoredContentInfo
  
class (Base a, HasSelector a) => HyperLink a where
  hyper_linkage_type :: a -> Word8
  link_destination_type :: a -> Word8 -- 
--  selector_length :: a -> Word8
--  selector_bytes    :: a -> [Word8]
  link_destination :: a -> LinkDestination
  private_data :: a -> [Word8]

class TargetRegionSpec a where
  pefecture_bitmap :: a -> Word64

data TargetRegionSpecData = MkTargetRegionSpec {
  _pefecture_bitmap :: Word64
  }
  
instance TargetRegionSpec TargetRegionSpecData where
  pefecture_bitmap = _pefecture_bitmap
  
class Base a => TargetRegion a where
  region_spec_type :: a -> Word8 -- only 0x01
  specs :: a -> Maybe [TargetRegionSpecData]

class HasSelector a where
  selector_length   :: a -> Word8
  selector_bytes    :: a -> [Word8]  

class (Base a, HasISO_639_LanguageCode a, HasTextAndLen a, HasSelector a, HasDataComponentID a) => DataContents a where
--  data_component_id :: a -> Word16
  entry_component   :: a -> Word8
--  selector_length   :: a -> Word8
--  selector_bytes    :: a -> [Word8]
  num_of_component_ref :: a -> Word8
  component_ref :: a -> [Word8]
--  iso_639_language_code :: a -> LangCode
--  text_length :: a -> Word8
--  text :: a -> String
  
class Base a => VideoDecodeControl a where
  still_picture_flag :: a -> Bool
  sequence_end_code_flag :: a -> Bool
  video_encode_format :: a -> Word8
--  reserved_future_use :: a -> Word8 -- 2
  
class Base a => TerrestrialDeliverySystem a where
  
class Base a => PartialReception a where
  
class Base a => Series a where
  
class Base a => EventGroup a where
  
class Base a => SI_TransmissionParameter a where
  
class Base a => BroadcasterName a where
  
class (Base a, HasText a, HasComponent a) => ComponentGroup a where
--  reserved_future_use :: a -> Word8
--  stream_content :: a -> Word8
--  component_type :: a -> Word8
--  component_tag :: a -> Word8
--  iso_639_language_code :: a -> LangCode
--  text :: a -> String
  
  
class Base a => SI_Prime_TS a where
  
class Base a => BoardInformation a where
  
class Base a => LDT_Link a where
  
class Base a => ConnectedTransmission a where
  
class Base a => TS_Information a where
  
class Base a => ExtensionBroadcaster a where
  
class Base a => LogoTransmission a where
  
class Base a => ContentAvailability a where
  
class Base a => CarouselCompatibleComposite a where
  
class Base a => ConditionalPlayback a where
  
class Base a => AVC_Video a where
  
class Base a => AVC_Timing_HRD a where
  
class Base a => ServiceGroup a where

class Base a => SystemManagement a where
  broadcasting_flag :: a -> Word8
  broadcasting_identifier :: a -> Word8
  additional_broadcasting_identifier :: a -> Word8
  additional_identification_info :: a -> [Word8]
  
