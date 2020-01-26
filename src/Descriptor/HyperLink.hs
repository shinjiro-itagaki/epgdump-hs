module Descriptor.HyperLink (
  Class(..)
  ,Data
  ) where
import Data.Word(Word64, Word32, Word16, Word8)  
import Common(ByteString)
import qualified Descriptor.Base as Base
import qualified Descriptor.Header as Header
import Data.Vector(Vector,empty,toList,snoc)

import qualified Descriptor.Link.ContentInfo       as ContentInfo
import qualified Descriptor.Link.ContentModuleInfo as ContentModuleInfo
import qualified Descriptor.Link.ErtNodeInfo       as ErtNodeInfo
import qualified Descriptor.Link.EventInfo         as EventInfo
import qualified Descriptor.Link.ModuleInfo        as ModuleInfo
import qualified Descriptor.Link.ServiceInfo       as ServiceInfo
import qualified Descriptor.Link.StoredContentInfo as StoredContentInfo

class (Base.Class a) => Class a where
  hyper_linkage_type    :: a -> Word8
  link_destination_type :: a -> Word8
  selector_length       :: a -> Word8
  selector_bytes        :: a -> ByteString
  private_data          :: a -> ByteString
  link_destination      :: a -> LinkDestination  

data Data = MkData {
  _header                :: Header.Data,
  _hyper_linkage_type    :: Word8,
  _link_destination_type :: Word8,
  _selector_length       :: Word8,
  _selector_bytes        :: ByteString,
  _private_data          :: ByteString
  } deriving (Show)

instance Header.Class Data where
  header = _header

instance Base.Class Data where

instance Class Data where  
  hyper_linkage_type    = _hyper_linkage_type
  link_destination_type = _link_destination_type 
  selector_length       = _selector_length
  selector_bytes        = _selector_bytes
  private_data          = _private_data
  link_destination    x = Null

data LinkDestination =
  Null
  | MkLinkService         ServiceInfo.Data
  | MkLinkEvent         EventInfo.Data
  | MkLinkModule        ModuleInfo.Data
  | MkLinkContent       ContentInfo.Data
  | MkLinkContentModule ContentModuleInfo.Data
  | MkLinkErtNode       ErtNodeInfo.Data
  | MkLinkStoredContent StoredContentInfo.Data



