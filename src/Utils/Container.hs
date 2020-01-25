{-# LANGUAGE FlexibleInstances #-}

module Utils.Container where
import Data.Sequence(Seq)


class Class a where
  append :: a -> b -> a
