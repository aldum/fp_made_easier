module Ch6.Data where

import Ch6.Types

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
