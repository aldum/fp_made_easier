module Ch7.Ch7b where

import Ch7.Types
import Prelude

import Data.Generic.Rep (from)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), split)
import Data.Int (fromString) as ParseInt
import Effect (Effect)
import Effect.Console (log)
import Undefined (undefined)

test :: Effect Unit
test = do
  log $ show $ person
  log $ show $ toCSV $ person
  log $ show $ toCSV
    (Person
      { name: FullName "Sue Smith"
      , age: Age 23
      , occupation: Doctor
      }) == CSV "Sue Smith,23 years,Doctor"

person :: Person
person = Person
  { name: FullName "Roger Davis"
  , age: Age 27
  , occupation: Dentist
  }

newtype CSV = CSV String
derive instance newtypeCSV :: Newtype CSV _
derive newtype instance eqCSV :: Eq CSV
derive newtype instance showCSV :: Show CSV

class ToCSV a where
  toCSV :: a -> CSV

instance personToCsv :: ToCSV Person where
  toCSV (Person {name, age, occupation}) =
    CSV $ show name <> ","
       <> show age <> ","
       <> show occupation

-- 7.27
class FromCSV a where
  fromCSV :: CSV -> Maybe a

instance personFromCsv :: FromCSV Person where
  fromCSV (CSV s) = case (split (Pattern ",") s) of
    -- [n, a, o] -> case (fromString o) of
    --   Just occ ->  case (ParseInt.fromString a) of
    --       Just i    -> Just (Person { name: FullName n, age: Age i, occupation: occ })
    --       Nothing   -> Nothing
    --   Nothing  -> Nothing
    [n, a, o] -> do
      age' <- ParseInt.fromString a
      occ' <- fromString o
      pure $ Person { name: FullName n, age: Age age', occupation: occ' }
    _ -> Nothing
