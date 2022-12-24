module Ch7.Ch7a where

import Data.Eq (class Eq)
import Data.Ord (class Ord, Ordering(..), compare)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, discard, (==), ($), (<), (>), (<=))
import Undefined (undefined)

-- data Maybe a = Nothing | Just a
data Option a = None | Some a -- let's Scala this up a bit

someInt :: Option Int
someInt = Some 1

none :: ∀ a. Option a
none = None

test :: Effect Unit
test = do
  -- log $ show $ "Some 5"
  log $ show $ Some 5 == Some 5 -- ❶
  log $ show $ Some 5 == Some 2 -- ❷ Prints false.
  log $ show $ Some 5 == None -- ❸ Prints false.
  log $ show $ None == Some 5 -- ❹ Prints false.
  log $ show $ None == (None :: Option Unit) -- ❺ Prints true.
  log "------------------"
  log $ show $ Some 1 < Some 5 -- ❷ true
  log $ show $ Some 5 <= Some 5 -- ❸ true
  log $ show $ Some 5 > Some 10 -- ❹ false
  log $ show $ Some 10 >= Some 10 -- ❺ true
  log $ show $ Some 99 > None -- ❻ true
  log $ show $ Some 99 < None -- ❼ false

-- derive instance optionEq :: Eq a => Eq (Option a)
instance optionEq :: Eq a => Eq (Option a) where
  eq (Some a1) (Some a2) = a1 == a2
  eq None      None      = true
  eq _         _         = false

-- derive instance optionOrd :: Ord a => Ord (Option a)
instance optionOrd :: Ord a => Ord (Option a) where
  -- compare o1 o2 = undefined
  compare None     None     = EQ
  compare None     (Some _) = LT
  compare (Some _) None     = GT
  compare (Some a) (Some b) = a `compare` b


greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq x y | x == y = true
greaterThanOrEq x y | x > y  = true
greaterThanOrEq _ _          = false

infixl 4 greaterThanOrEq as >=

