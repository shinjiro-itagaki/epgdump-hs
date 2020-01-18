{-# LANGUAGE FlexibleInstances #-}

module Descriptor.Common(
  HasName(..)
  ,HasText(..)
  ,HasTitle(..)
  ,HasPrivateDataBytes(..)
  ,HasReferenceServiceID(..)
  ,HasComponentTag(..)
  ,HasComponent(..)  
  ,HasDataComponentID(..)
  ,HasModuleID(..)
  ,HasContentID(..)
  ) where
import Common(HasOriginalNetworkID(..),HasServiceID(..))
import Data.Word(Word64, Word32, Word16, Word8)
import Data.ByteString(ByteString)

class HasName a where
  name :: a -> String

class HasText a where
  text :: a -> String

class HasTitle a where
  title :: a -> String
--  title_length :: a -> Word8

class HasPrivateDataBytes a where
  private_data_bytes :: a -> [Word8]

class HasReferenceServiceID a where
  reference_service_id :: a -> Word16

class HasComponentTag a where
  component_tag :: a -> Word8

class (HasComponentTag a) => HasComponent a where
  stream_content :: a -> Word8
  component_type :: a -> Word8

class HasDataComponentID a where
  data_component_id :: a -> Word16

class HasModuleID a where
  module_id :: a -> Word16

class HasContentID a where
  content_id :: a -> Word32
