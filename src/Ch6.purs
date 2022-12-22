module Ch6 where

import Prelude

import Undefined (undefined)

type Address =
  { street1 :: String
  , street2 :: String
  , city :: String
  , state :: String
  , zip :: String
  }

type Directions = {}

getDirections :: Address -> Directions
getDirections _ = undefined


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


getPersonDirections :: Person -> Directions
getPersonDirections (Person p) = getDirections p.address

getCompanyDirections :: Company -> Directions
getCompanyDirections (Company c) = getDirections c.address

getResidenceDirections :: Residence -> Directions
getResidenceDirections residence =
  getDirections $ case residence of
    Home address -> address
    Facility address -> address

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
