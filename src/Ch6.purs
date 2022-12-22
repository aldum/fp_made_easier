module Ch6 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype, unwrap)
import Undefined (undefined)


type Address =
  { street1 :: String
  , street2 :: String
  , city :: String
  , state :: String
  , zip :: String
  }

class HasAddress a where
  getAddress :: a -> Address


type Directions = {}

data Person = Person
  { name :: String
  , age :: Int
  , address :: Address
  }
data Company = Company
  { name :: String
  , address :: Address
  }
data Residence
  = Home Address
  | Facility Address

data EmptyLot = EmptyLot
  { daysEmpty :: Int
  , price :: Int
  , address :: Address
  }

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
newtype Janitor = Janitor Person

-- -------------------
--       data      ---
-- -------------------

person :: Person
person = Person
  { name: "Joe Mama"
  , age: 22
  , address:
    { street1: "123 Main Street"
    , street2: "Apt 152"
    , city: "Jamestown"
    , state: "CA"
    , zip: "95327"
    }
  }
company :: Company
company = Company
  { name: "Acme"
  , address:
    { street1: "987 Tesla Way"
    , street2: "Suite 101"
    , city: "Irvine"
    , state: "CA"
    , zip: "92602"
    }
  }

-- Buford, WY has population of 1
home :: Residence
home = Home
  { street1: "1 1st Street"
  , street2: "Apt 1"
  , city: "Buford"
  , state: "WY"
  , zip: "82052"
  }
facility :: Residence
facility = Facility
  { street1: "54321 Countdown Ave"
  , street2: ""
  , city: "Huntsville"
  , state: "AL"
  , zip: "35805"
  }
