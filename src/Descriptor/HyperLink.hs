module Descriptor.HyperLink where
import Descriptor.Common(
  Base
  ,HasSelector
  ,HasOriginalNetworkID(..)
  ,TOS(..)
  ,HasServiceID(..)
  ,HasOriginalNetworkID(..)
  ,HasEventID(..)
  ,HasComponentTag(..)
  ,HasModuleID(..)
  ,HasContentID(..)
  ,HasComponentTag(..)
  ,HasModuleID(..)
  )
import Data.Word(Word64, Word32, Word16, Word8)  
import Data.ByteString(ByteString)

class (Base a, HasSelector a) => HyperLink a where
  hyper_linkage_type :: a -> Word8
  link_destination_type :: a -> Word8 -- 
--  selector_length :: a -> Word8
--  selector_bytes    :: a -> [Word8]
  link_destination :: a -> LinkDestination
  private_data :: a -> [Word8]


data LinkDestination =
  MkLinkService         LinkServiceInfo
  | MkLinkEvent         LinkEventInfo
  | MkLinkModule        LinkModuleInfo
  | MkLinkContent       LinkContentInfo
  | MkLinkContentModule LinkContentModuleInfo
  | MkLinkErtNode       LinkErtNodeInfo
  | MkLinkStoredContent LinkStoredContentInfo


data LinkServiceInfo = MkLinkServiceInfo {
  _link_service_info_original_network_id :: Word16,
  _link_service_info_transport_stream_id :: Word16,
  _link_service_info_service_id :: Word16
  } -- 0x01

  
data LinkEventInfo = MkLinkEventInfo {
  _link_event_info_original_network_id :: Word16,
  _link_event_info_transport_stream_id :: Word16,
  _link_event_info_service_id :: Word16,
  _link_event_info_event_id :: Word16
  } -- 0x02
  
data LinkModuleInfo  = MkLinkModuleInfo {
  _link_module_info_original_network_id :: Word16,
  _link_module_info_transport_stream_id :: Word16,
  _link_module_info_service_id :: Word16,
  _link_module_info_event_id :: Word16,
  _link_module_info_component_tag :: Word8,
  _link_module_info_module_id :: Word16
    } -- 0x03

data LinkContentInfo = MkLinkContentInfo {
  _link_content_info_original_network_id :: Word16,
  _link_content_info_transport_stream_id :: Word16,
  _link_content_info_service_id :: Word16,
  _link_content_info_content_id :: Word32
  } -- 0x04


data LinkContentModuleInfo = LinkContentModuleInfo {
  _link_content_module_original_network_id :: Word16,
  _link_content_module_transport_stream_id :: Word16,
  _link_content_module_service_id :: Word16,
  _link_content_module_content_id :: Word32,
  _link_content_module_component_tag :: Word8,
  _link_content_module_module_id :: Word16
  } -- 0x05


data LinkErtNodeInfo = MkLinkErtNodeInfo {
  information_provider_id :: Word16,
  event_relation_id :: Word16
  } -- 0x06
  
data LinkStoredContentInfo = MkLinkStoredContentInfo {
  uri :: String
  } -- 0x07



  
instance HasOriginalNetworkID LinkServiceInfo where
  original_network_id = _link_service_info_original_network_id

instance TOS LinkServiceInfo where
  transport_stream_id = _link_service_info_transport_stream_id
  
instance HasServiceID LinkServiceInfo where  
  service_id = _link_service_info_service_id

instance HasOriginalNetworkID LinkEventInfo where
  original_network_id = _link_event_info_original_network_id

instance TOS LinkEventInfo where
  transport_stream_id = _link_event_info_transport_stream_id
  
instance HasServiceID LinkEventInfo where  
  service_id = _link_event_info_service_id

instance HasEventID LinkEventInfo where
  event_id = _link_event_info_event_id


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


instance HasOriginalNetworkID LinkContentInfo where
  original_network_id = _link_content_info_original_network_id

instance TOS LinkContentInfo where
  transport_stream_id = _link_content_info_transport_stream_id
  
instance HasServiceID LinkContentInfo where  
  service_id = _link_content_info_service_id

instance HasContentID LinkContentInfo where
  content_id = _link_content_info_content_id

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


