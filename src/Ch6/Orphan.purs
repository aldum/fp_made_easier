module Orphan where

import Prelude

import Ch6 (class Combine)

import Data.Newtype (class Newtype)

-- instance combineInt :: Combine Int where -- ORPHANED INSTANCE!!
--   combine = (+)


x' :: AddInt
x' = AddInt 10

y' :: Int
y' = 11

-- z :: Int
-- z = x + y -- COMPILER ERROR!!
z :: AddInt
z = (AddInt 7) + (AddInt 3) -- COMPILER ERROR!!

newtype AddInt = AddInt Int
instance combineAddInt :: Combine AddInt where
  combine (AddInt x) (AddInt y) = AddInt (x + y)

-- instance semiringAddInt :: Semiring AddInt where
--   add (AddInt x) (AddInt y) = AddInt (x + y)
--   zero = AddInt 0
--   mul (AddInt x) (AddInt y) = AddInt (x * y)
--   one = AddInt 1

derive instance newtypeAddInt :: Newtype AddInt _
derive newtype instance semiringAddInt :: Semiring AddInt
derive newtype instance ringAddInt :: Ring AddInt
derive newtype instance commutativeRingAddInt :: CommutativeRing AddInt
derive newtype instance euclideanRingAddInt :: EuclideanRing AddInt

