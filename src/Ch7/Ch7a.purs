module Ch7.Ch7a where

import Data.Eq (class Eq)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, discard, (==), ($))
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
  log $ show $ Some 5 == Some 5 -- COMPILER ERROR!! ❶
  log $ show $ Some 5 == Some 2 -- ❷ Prints false.
  log $ show $ Some 5 == None -- ❸ Prints false.
  log $ show $ None == Some 5 -- ❹ Prints false.
  log $ show $ None == (None :: Option Unit) -- ❺ Prints true.

-- derive instance optionEq :: Eq a => Eq (Option a)
instance optionEq :: Eq a => Eq (Option a) where
  eq (Some a1) (Some a2) = a1 == a2
  eq None      None      = true
  eq _         _         = false

