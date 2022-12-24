module Ch7.Ch7a where

import Data.Eq (class Eq, (==))
import Data.Ord (class Ord, Ordering(..), compare, (<), (<=), (>))
import Data.Show (class Show, show)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, ($), (<>), (||))
import Undefined (undefined)

-- data Maybe a = Nothing | Just a
data Option a = None | Some a -- let's Scala this up a bit

someInt :: Option Int
someInt = Some 1

none :: ∀ a. Option a
none = None

test :: Effect Unit
test = do
  log $ show $ Some "abc" -- ❶ Prints (Some "abc")
  log $ show $ (None :: Option Unit) -- ❸ Prints None.

-- 7.12
-- instance optionShow :: Show a => Show (Option a) where
--   show (Some s) = "Some(" <> show s <> ")"
--   show None     = "None"
--  derive instance optionShow :: Show a => Show (Option a)
derive instance optionShow :: Show (Option a)

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
greaterThanOrEq x y | x > y  = true
greaterThanOrEq _ _          = false

greaterThanOrEq' :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq' x y =
  let
    cmp = compare x y
  in
    cmp == GT || cmp == EQ

infixl 4 greaterThanOrEq as >=

