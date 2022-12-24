module Ch7.Types where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show (class Show, show)
import Data.Show.Generic (genericShow)
import Prelude ((<>))
import Undefined (undefined)

newtype FullName = FullName String

newtype Age = Age Int

data Occupation = Doctor | Dentist | Lawyer | Unemployed

fromString :: String -> Maybe Occupation
fromString = case _ of
  "Doctor"     -> Just Doctor
  "Dentist"    -> Just Dentist
  "Lawyer"     -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _            -> Nothing


data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

-- derive instance genericFullname :: Generic (FullName) _
-- instance showFullname :: Show FullName where
--   show = genericShow
instance showFullname :: Show FullName where
  show (FullName name) = name

-- derive instance genericAge :: Generic Age _
-- instance showAge :: Show Age where
--   show = genericShow
instance showAge :: Show Age where
  show (Age n) = show n <> " years"
-- derive instance newtypeAge :: Newtype Age _
-- derive newtype instance showAge :: Show Age

derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
  show = genericShow

derive instance genericPerson :: Generic Person _
instance showPerson :: Show Person where
  show = genericShow
