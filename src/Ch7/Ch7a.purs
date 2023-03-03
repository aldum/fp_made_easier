module Ch7.Ch7a where

import Data.Eq (class Eq, (==))
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord, Ordering(..), compare, (>))
import Data.Show (class Show, show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, ($), (||))
import Undefined (undefined)

-- data Maybe a = Nothing | Just a
data Option a = None | Some a -- let's Scala this up a bit
-- 7.18
data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

test :: Effect Unit
test =
  let
    x = Left "left" :: MyEitherVar

    y :: MyEitherVar
    y = Right $ Some 41
  in
    do
      log $ show x
      log $ show y

type MyEitherVar = Either String (Option Int)

-- 7.12
-- instance optionShow :: Show a => Show (Option a) where
--   show (Some s) = "Some(" <> show s <> ")"
--   show None     = "None"
derive instance genericOption :: Generic (Option a) _
instance showOption :: Show a => Show (Option a) where
  show = genericShow

-- 7.5
derive instance optionEq :: Eq a => Eq (Option a)
-- instance optionEq :: Eq a => Eq (Option a) where
--   eq (Some a1) (Some a2) = a1 == a2
--   eq None      None      = true
--   eq _         _         = false

derive instance optionOrd :: Ord a => Ord (Option a)

-- instance optionOrd :: Ord a => Ord (Option a) where
--   -- compare o1 o2 = undefined
--   compare None     None     = EQ
--   compare None     (Some _) = LT
--   compare (Some _) None     = GT
--   compare (Some a) (Some b) = a `compare` b

greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq x y | x == y = true
greaterThanOrEq x y | x > y = true
greaterThanOrEq _ _ = false

greaterThanOrEq' :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq' x y =
  let
    cmp = compare x y
  in
    cmp == GT || cmp == EQ

infixl 4 greaterThanOrEq as >=
