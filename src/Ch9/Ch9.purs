module Ch9.Ch9 where

import Data.Eq (class Eq, (==))
import Data.Generic.Rep (class Generic)
import Data.Show (show, class Show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, ($))
import Undefined (undefined)

test :: Effect Unit
test = do
  log $ show $ mempty <> ATrue == ATrue -- ❶ Prints true
  log $ show $ mempty <> AFalse == ATrue -- ❷ Prints false
  verifyAndBoolSemigroup

class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

-- 9.4
class Semigroup a <= Monoid a where
  mempty :: a

-- 9.6
data AndBool = AFalse | ATrue
derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _

instance showAndBool :: Show AndBool where
  show = genericShow

and :: AndBool -> AndBool -> AndBool
and AFalse _ = AFalse
and ATrue  a = a

infixr 3 and as &&

appendB :: AndBool -> AndBool -> AndBool
appendB ATrue ATrue = ATrue
appendB _     _     = AFalse

instance sgAndBool :: Semigroup AndBool where
  append a b = a && b
  -- append = appendB

-- 9.9
instance monoidAndBool :: Monoid AndBool where
  mempty = ATrue

-- 9.11
verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
  log "Verifying AndBool Semigroup Laws (1 test)"
            -- evaluate to true if a • (b • c) = (a • b) • c
  log $ show $ semigroupL ATrue AFalse ATrue

semigroupL :: AndBool -> AndBool-> AndBool -> Boolean
semigroupL a b c = a <> (b <> c) == (a <> b) <> c
