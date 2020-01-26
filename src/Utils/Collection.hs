{-# LANGUAGE FlexibleInstances #-}

module Utils.Collection where
import Common(EmptyExist)

class (EmptyExist a) => Class a where
  add :: a -> e -> a

