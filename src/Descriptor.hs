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

class HasTitle a where
  title :: a -> String
  title_length :: a -> Word8

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

class HasOriginalNetworkID a where
  original_network_id :: a -> Word16

class (HasServiceID a, HasOriginalNetworkID a) => TOS a where
  transport_stream_id :: a -> Word16
--  original_network_id :: a -> Word16
--  service_id          :: a -> Word16

class HasPrivateDataBytes a where
  private_data_bytes :: a -> [Word8]
  
class (Base a, TOS a, HasPrivateDataBytes a) => Linkage a where
--  transport_stream_id :: a -> Word16
--  original_network_id :: a -> Word16
--  service_id          :: a -> Word16
  linkage_type        :: a -> Word8
--  private_data_bytes  :: a -> [Word8]

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

instance HasOriginalNetworkID TOSData where
  original_network_id = _original_network_id

instance TOS TOSData where
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

instance HasOriginalNetworkID MosaicItem where
  original_network_id = original_network_id . tosdata

instance TOS MosaicItem where
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

class HasUserDefined a where
  user_defined                   :: a -> Word8
  
class (HasUserDefined a) => ComponentControl a where
  digital_recording_control_data :: a -> Word8
  maximum_bitrate_flag           :: a -> Bool
--  reserved_future_use            :: a -> Bool
--  user_defined                   :: a -> Word8
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
  maximum_bitrate                = _maximum_bitrate
  
instance HasUserDefined ComponentControlData where
  user_defined                   = _user_defined

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

instance HasOriginalNetworkID LinkServiceInfo where
  original_network_id = _link_service_info_original_network_id

instance TOS LinkServiceInfo where
  transport_stream_id = _link_service_info_transport_stream_id
  
instance HasServiceID LinkServiceInfo where  
  service_id = _link_service_info_service_id
  
data LinkEventInfo = MkLinkEventInfo {
  _link_event_info_original_network_id :: Word16,
  _link_event_info_transport_stream_id :: Word16,
  _link_event_info_service_id :: Word16,
  _link_event_info_event_id :: Word16
  } -- 0x02

instance HasOriginalNetworkID LinkEventInfo where
  original_network_id = _link_event_info_original_network_id

instance TOS LinkEventInfo where
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

instance HasOriginalNetworkID LinkModuleInfo where
  original_network_id = _link_module_info_original_network_id

instance TOS LinkModuleInfo where
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

instance HasOriginalNetworkID LinkContentInfo where
  original_network_id = _link_content_info_original_network_id

instance TOS LinkContentInfo where
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

instance HasOriginalNetworkID LinkContentModuleInfo where
  original_network_id = _link_content_module_original_network_id

instance TOS LinkContentModuleInfo where
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
  MkLinkService         LinkServiceInfo
  | MkLinkEvent         LinkEventInfo
  | MkLinkModule        LinkModuleInfo
  | MkLinkContent       LinkContentInfo
  | MkLinkContentModule LinkContentModuleInfo
  | MkLinkErtNode       LinkErtNodeInfo
  | MkLinkStoredContent LinkStoredContentInfo

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
  area_code :: a -> AreaCode
  guard_interval :: a -> Word8
  transmission_mode :: a -> Word8
  frequencies :: a -> [Word16]

class Base a => PartialReception a where
  service_ids :: a -> [Word16]
  
class Base a => Series a where
  series_id :: a -> Word16
  repeat_label :: a -> Word8
  program_pattern :: a -> Word8
  expire_date_valid_flag :: a -> Bool
  expire_date :: a -> Word16
  episode_number :: a -> Word16
  last_episode_number :: a -> Word16
  series_name :: a -> String

data Event = MkEvent {
  _event_service_id :: Word16,
  _event_event_id :: Word16
  }
  
data EventGroupData = MkEventGroupData {
  _event_group_original_network_id :: Word16,
  _event_group_transport_stream_id :: Word16,
  _event_group_service_id :: Word16,
  _event_group_event_id :: Word16
  }

instance HasOriginalNetworkID EventGroupData where
  original_network_id = _event_group_original_network_id

instance TOS EventGroupData where
  transport_stream_id = _event_group_transport_stream_id
  
instance HasServiceID EventGroupData where
  service_id = _event_group_service_id

instance HasEventID EventGroupData where
  event_id = _event_group_event_id

class HasMaybePrivateDataBytes a where
  maybe_private_data_bytes :: a -> Maybe [Word8]
  
class (Base a, HasMaybePrivateDataBytes a) => EventGroup a where
  group_type :: a -> Word8
  event_count :: a -> Word8
  maybe_event :: a -> Maybe [Event]
  maybe_group_data :: a -> Maybe [EventGroupData]
--  maybe_private_data_bytes :: a -> Maybe [Word8]  

data SI_TableDescription = MkSI_TableDescription{
  table_id :: Word8,
  table_description_length :: Word8,
  table_description_bytes :: [Word8]
  }

class Base a => SI_Parameter a where
  parameter_version :: a -> Word8
  update_time :: a -> Word16
  table_descriptions :: a -> [SI_TableDescription]
  
class (Base a, HasName a) => BroadcasterName a where

data CA_Unit = MkCA_Unit {
  ca_unit_id :: Word8,
  num_of_component :: Word8,
  component_tags :: [Word8]
  }
  
data ComponentGroupData = MkComponentGroupData {
  component_group_id :: Word8,
  num_of_ca_unit :: Word8,
  ca_units :: [CA_Unit],
  total_bit_rate :: Maybe Word8,
  _component_group_data_text_length :: Word8,
  _component_group_data_text :: String
  }

instance HasText ComponentGroupData where
  text = _component_group_data_text

instance HasTextAndLen ComponentGroupData where
  text_length = _component_group_data_text_length
  
  
class (Base a, HasText a, HasComponent a) => ComponentGroup a where
  component_group_type :: a -> Word8
  total_bit_rate_flag :: a -> Bool
  num_of_group :: a -> Word8
  groups :: a -> [ComponentGroupData]
  
class (Base a, SI_Parameter a) => SI_Prime_TS a where
--  parameter_version :: a -> Word8
--  update_time :: a -> Word16
  si_prime_ts_network_id :: a -> Word16
  si_prime_transport_stream_id :: a -> Word16
--  table_descriptions :: [SI_TableDescription]
  
class (Base a, HasTitle a, HasTextAndLen a) => BoardInformation a where
--  title :: a -> String
--  title_length :: a -> Word8

data LDT_LinkDesc = MkLDT_LinkDesc {
  description_id :: Word16,
--  reserved_future_use :: Word8,
  description_type :: Word8,
  _LDT_LinkDesc_user_defined :: Word8
  }

instance HasUserDefined LDT_LinkDesc where
  user_defined = _LDT_LinkDesc_user_defined
  
class (Base a, TOS a) => LDT_Link a where
  original_service_id :: a -> Word16
  original_service_id = service_id
  descriptions :: a -> [LDT_LinkDesc]
  
class Base a => ConnectedTransmission a where
  connected_transmission_group_id :: a -> Word16
  segment_type :: a -> Word8
  modulation_type_a :: a -> Word8
  modulation_type_b :: a -> Word8
--  reserved_future_use :: Word8,
  additional_connected_transmission_infos :: a -> [Word8]

data TransmissionType = MkTransmissionType {
  transmission_type_info :: Word8,
  num_of_service :: Word8,
  services :: [Word16]
  }
  
class Base a => TS_Information a where
  remote_control_key_id :: a -> Word8
  length_of_ts_name :: a -> Word8
  transmission_type_count :: a -> Word8
  ts_name :: a -> String
  transmission_types :: a -> [TransmissionType]

data BroadcasterAffiliation = MkBroadcasterAffiliation {
  affiliation_id :: Word8
  }

data BroadcasterInfo = MkBroadcasterInfo {
  broadcaster_id :: Word8,
  _broadcaster_original_network_id :: Word16
  }  

instance HasOriginalNetworkID BroadcasterInfo where
  original_network_id = _broadcaster_original_network_id

class (HasPrivateDataBytes a) => BroadcasterCommon a where
  number_of_broadcaster_id_loop :: a -> Word8
  info :: a -> [BroadcasterInfo]
--  private_data_bytes :: a -> [Word8]

data Broadcaster =
  MkTV {
  terrestrial_broadcaster_id :: Word16,
  number_of_affiliation_id_loop :: Word8,
  _tv_number_of_broadcaster_id_loop :: Word8,
  affiliation_ids :: [Word8],
  _tv_info :: [BroadcasterInfo],
  _tv_private_data_bytes :: [Word8]
  } |
  MkSd {
  terrestrial_sound_broadcaster_id :: Word16,
  number_of_sound_broadcast_affiliation_id_loop :: Word8,
  _sd_number_of_broadcaster_id_loop :: Word8,
  sound_broadcast_affiliation_ids :: [Word8],
  _sd_info :: [BroadcasterInfo],
  _sd_private_data_bytes :: [Word8]
  }

instance HasPrivateDataBytes Broadcaster where
  private_data_bytes (MkTV {_tv_private_data_bytes = x}) = x
  private_data_bytes (MkSd {_sd_private_data_bytes = x}) = x

instance BroadcasterCommon Broadcaster where
  number_of_broadcaster_id_loop (MkTV {_tv_number_of_broadcaster_id_loop = x}) = x
  number_of_broadcaster_id_loop (MkSd {_sd_number_of_broadcaster_id_loop = x}) = x
  info (MkTV {_tv_info = x}) = x
  info (MkSd {_sd_info = x}) = x
  
class (Base a, HasMaybePrivateDataBytes a) => ExtendedBroadcaster a where
  broadcaster_type :: a -> Word8
-- reserved_future_use
  maybe_broadcaster :: a -> Maybe Broadcaster
  
class Base a => LogoTransmission a where
  logo_transmission_type :: a -> Word8
  logo_id :: a -> Maybe Word16
  logo_version :: a -> Maybe Word16
  download_data_id :: a -> Maybe Word16
  logo_char :: a -> Maybe String
  
class Base a => ContentAvailability a where
  copy_restriction_mode :: a -> Bool
  image_constraint_token :: a -> Bool
  retention_mode :: a -> Bool
  retention_state :: a -> Word8
  encryption_mode :: a -> Bool
-- reserved_future_use N  
  
class Base a => CarouselCompatibleComposite a where
  -- sub_scriptor
  
class Base a => ConditionalPlayback a where
  
class Base a => AVC_Video a where
  profile_idc :: a -> Word8
  constraint_set0_flag :: a -> Bool
  constraint_set1_flag :: a -> Bool  
  constraint_set2_flag :: a -> Bool
  avc_compatible_flags :: a -> Word8
  level_idc :: a -> Word8
  avc_still_present :: a -> Bool
  avc_24_hour_picture_flag :: a -> Bool
--  reserved :: a -> Word8
  
class Base a => AVC_Timing_HRD a where
  hrd_management_valid_flag :: a -> Bool
--  reserved :: a -> Word8
  picture_and_timing_info_present :: a -> Bool
  _90kHz_flag :: a -> Maybe Bool
-- reserved :: a -> Word8
  _N :: a -> Maybe Word32
  _K :: a -> Maybe Word32
  num_units_in_tick :: a -> Word32
  fixed_farme_rate_flag :: a -> Bool
  temporal_poc_flag :: a -> Bool
  picture_to_display_conversion_flag :: a -> Bool
-- reserved :: a -> Word8

data ServicePair = ServicePair {
  primary_service_id :: Word16,
  secondary_service_id :: Word16
  }

data ServiceGroupType = Simultaneous Word8 | Undefined Word8
  
class (Base a, HasMaybePrivateDataBytes a) => ServiceGroup a where
  service_group_type :: a -> ServiceGroupType
--  reserved_future_use
  service_group_data :: a -> Maybe [ServicePair]
--  maybe_private_data_bytes :: a -> Maybe [Word8]
  

class Base a => SystemManagement a where
  broadcasting_flag :: a -> Word8
  broadcasting_identifier :: a -> Word8
  additional_broadcasting_identifier :: a -> Word8
  additional_identification_info :: a -> [Word8]
  
