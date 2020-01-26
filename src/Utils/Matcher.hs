{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils.Matcher where
import Common

class (Eq a) => Class a where
  (=|==) :: a -> [a] -> Bool
  (=|==) x []     = False
  (=|==) x (y:[]) = (x == y)
  (=|==) x (y:ys) = (x == y) || x =|== ys

  (/=|==) :: a -> [a] -> Bool
  (/=|==) x y = not $  x =|== y
  
  (==|=) :: [a] -> a -> Bool
  (==|=) xs y = y =|== xs

  (/==|=) :: [a] -> a -> Bool
  (/==|=) x y = not $ x ==|= y

  (==|==) :: [a] -> [a] -> Bool
  (==|==) []     ys = False
  (==|==) (x:[]) ys = x =|== ys
  (==|==) (x:xs) ys = x =|== ys || xs ==|== ys

  (/==|==) :: [a] -> [a] -> Bool
  (/==|==) x y = not $ x ==|== y

instance Class PID
instance Class TableID

matchPID :: PIDs -> PID -> Bool
matchPID (MkPIDs        xs) y = xs ==|= y
matchPID (MkExcludePIDs []) y = True
matchPID (MkExcludePIDs xs) y = xs /==|= y
