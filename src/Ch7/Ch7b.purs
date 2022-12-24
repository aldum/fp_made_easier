module Ch7.Ch7b where

import Ch7.Types
import Prelude

import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Console (log)
import Undefined (undefined)

test :: Effect Unit
test = do
  log $ show $ person

person :: Person
person = Person
  { name: FullName "Roger Davis"
  , age: Age 27
  , occupation: Dentist
  }

newtype CSV = CSV String

class ToCSV a where
  toCSV :: a -> CSV

instance personToCsv :: ToCSV Person where
  toCSV (Person {name, age, occupation}) =
    CSV $ show name <> ","
       <> show age <> ","
       <> show occupation
