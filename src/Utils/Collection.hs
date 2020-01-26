{-# LANGUAGE FlexibleInstances #-}

module Utils.Collection where
import qualified Utils.EmptyExist as EmptyExist

class (EmptyExist.Class a) => Class a where
  add :: a -> e -> a

