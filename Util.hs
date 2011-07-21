module Util
  ( fAnd
  , tuplify2
  ) where

fAnd :: a -> [a -> Bool] -> Bool
fAnd x = all ($x)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)
