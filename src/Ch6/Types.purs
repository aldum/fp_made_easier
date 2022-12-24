module Ch6.Types where

type Address =
  { street1 :: String
  , street2 :: String
  , city :: String
  , state :: String
  , zip :: String
  }

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