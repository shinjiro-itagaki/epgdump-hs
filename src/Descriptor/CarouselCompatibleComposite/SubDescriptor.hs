module Descriptor.CarouselCompatibleComposite.SubDescriptor where

import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Expire          as Expire
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Name            as Name
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.ProviderPrivate as ProviderPrivate 
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.StoreRoot       as StoreRoot 
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Subdirectory    as Subdirectory
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Title           as Title
import qualified Descriptor.CarouselCompatibleComposite.SubDescriptor.Type            as Type


data Data =
  Null
  | MkExpire          Expire.Data
  | MkName            Name.Data
  | MkProviderPrivate ProviderPrivate.Data
  | MkStoreRoot       StoreRoot.Data 
  | MkSubdirectory    Subdirectory.Data
  | MkTitle           Title.Data
  | MkType            Type.Data
  deriving (Show)
