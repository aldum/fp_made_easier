module Ch7.Ch7b where

import Ch7.Types
import Prelude

import Data.Generic.Rep (from)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Data.Show.Generic as Show
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test =
    let testPerson = Person
            { name: FullName "Sue Smith"
            , age: Age 23
            , occupation: Doctor
            }
    in
      do
        log $ show $ (fromCSV $ toCSV testPerson :: Maybe Person)
        log $ show $ (toCSV testPerson # fromCSV) == Just testPerson
        log $ show $ toCSV person
        log $ show $ (toCSV person # fromCSV) == Just person
        log $ show $ test2

test2 :: Maybe Person
test2 = fromCSV (CSV "Jurgen,14 years,Unemployed")

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

-- derive newtype instance showMaybeCSV :: Show (Maybe CSV)


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
      age' <- parseAge a
      occ' <- fromString o
      pure $ Person { name: FullName n, age: age', occupation: occ' }
    _ -> Nothing
