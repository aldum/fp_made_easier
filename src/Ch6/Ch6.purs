module Ch6 where

import Prelude

import Ch6.Types
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)

-- import Undefined (undefined)


class HasAddress a where
  getAddress :: a -> Address


instance hasAddressPerson :: HasAddress Person where
  getAddress (Person p) = p.address

instance hasAddressCompany :: HasAddress Company where
  getAddress (Company c) = c.address

instance hasAddressResidence :: HasAddress Residence where
  getAddress (Home address) = address
  getAddress (Facility address) = address

instance hasAddressEmptyLot :: HasAddress EmptyLot where
  getAddress (EmptyLot l) = l.address

-- getDirections :: ∀ a. HasAddress => a -> Directions
-- getDirections hasAddr = let address = getAddress hasAddr in
--   undefined

data SomeType = This | That | TheOther | AndYetAnother

derive instance eqSomeType :: Eq SomeType
derive instance ordSomeType :: Ord SomeType

derive instance genericSomeType :: Generic SomeType _
instance showSomeType :: Show SomeType where
  show = genericShow

-- 6.7
newtype FirstName = FirstName String

derive instance newTypeFirstName :: Newtype FirstName _
derive instance eqFirstName :: Eq FirstName

newtype LastName = LastName String

derive instance newTypeLastName :: Newtype LastName _

glueNames
  :: ∀ a b
   . Newtype a String
  => Newtype b String
  => String
  -> a
  -> b
  -> String
glueNames between n1 n2 = unwrap n1 <> between <> unwrap n2

lastNameFirst :: LastName -> FirstName -> String
lastNameFirst = glueNames ", "

fullName :: FirstName -> LastName -> String
fullName = glueNames " "

fullName' :: FirstName -> LastName -> String
fullName' first last = unwrap first <> " " <> unwrap last

newtype Ceo = Ceo Person

derive instance newtypeCeo :: Newtype Ceo _
derive newtype instance hasAddressCeo :: HasAddress Ceo

newtype Janitor = Janitor Person

derive instance newtypeJanny :: Newtype Janitor _
derive newtype instance hasAddressJanitor :: HasAddress Janitor

genericPersonHasAddress :: ∀ a. Newtype a Person => a -> Address
genericPersonHasAddress wrappedPerson =
  getAddress $ unwrap wrappedPerson

-- 6.9
data Overlap = Overlap

-- instance overlapShow1 :: Show Overlap where
--   show = "Overlap 1"
-- instance overlapShow2 :: Show Overlap where -- COMPILER ERROR!!
--   show = "Overlap 2"

class Combine a where
  combine :: a -> a -> a

instance combineInt :: Combine Int where
  combine = (+)

-- instance combineMultInt :: Combine Int where -- OVERLAPPING INSTANCES!!
--   combine = (*)

newtype AddInt = AddInt Int
newtype MultInt = MultInt Int

instance combineAddInt :: Combine AddInt where
  combine (AddInt x) (AddInt y) = AddInt (x + y)

instance combineMultInt :: Combine MultInt where
  combine (MultInt x) (MultInt y) = MultInt (x * y)

class IsRecord a where
  isRecord :: a -> Boolean

instance isRecordRecord :: IsRecord (Record a) where
  isRecord _ = true
-- instance isRecordOther :: IsRecord a where -- COMPILER ERROR ❸
--   isRecord _ = false
else instance isRecordOther :: IsRecord a where
  isRecord _ = false
