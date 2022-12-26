module Ch9.Ch9 where

import Data.Eq (class Eq, (==))
import Data.Generic.Rep (class Generic)
import Data.Show (show, class Show)
import Data.Show.Generic (genericShow)

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, ($), (&&))
import Undefined (undefined)

test :: Effect Unit
test = do
  log $ show $ mempty <> ATrue == ATrue -- ❶ Prints true
  log $ show $ mempty <> AFalse == ATrue -- ❷ Prints false
  verifyAndBoolSemigroup
  verifyAndBoolMonoid
  verifyOrBoolSemigroup
  verifyOrBoolMonoid


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

-- infixr 3 and as &&

appendB :: AndBool -> AndBool -> AndBool
appendB ATrue ATrue = ATrue
appendB _     _     = AFalse

instance sgAndBool :: Semigroup AndBool where
  append a b = a `and` b
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

semigroupL :: ∀ a. Monoid a => Eq a => a -> a-> a -> Boolean
semigroupL a b c = a <> (b <> c) == (a <> b) <> c

-- 9.13
verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
  log "Verifying AndBool Monoid Laws (2 tests)"
  log $ show $ monoidL ATrue  -- ❶ a • e = e • a = a -- ATrue
  log $ show $ monoidL AFalse -- ❶ a • e = e • a = a -- AFalse

monoidL :: ∀ a. Monoid a => Eq a => a -> Boolean
monoidL a = a <> mempty == mempty <> a
monoidL' :: AndBool -> Boolean
monoidL' a = a <> mempty == a && mempty <> a == a


-- 9.15
data OrBool = OFalse | OTrue
derive instance eqOrBool :: Eq OrBool
derive instance genericOrBool :: Generic OrBool _
instance showOrBool :: Show OrBool where
  show = genericShow

instance sgOrBool :: Semigroup OrBool where
  append OFalse a = a
  append OTrue  _ = OTrue

instance monoidOrBool :: Monoid OrBool where
  mempty = OFalse

-- 9.17
verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
  log "Verifying OrBool Semigroup Laws (1 test)"
            -- evaluate to true if a • (b • c) = (a • b) • c
  log $ show $ semigroupL OFalse OTrue OTrue

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do
  log "Verifying OrBool Monoid Laws (2 tests)"
  log $ show $ monoidL OTrue
  log $ show $ monoidL OFalse

-- 9.19
data Mod4 = Zero
          | One
          | Two
          | Three

-- 9.21
instance sgMod4 :: Semigroup Mod4 where
  append Zero m      = m
  append m    Zero   = m
  append One  Three  = Zero
  append One  One    = Two
  append One  Two    = Three
  append Two  One    = Three
  append Two  Two    = Zero
  append Two  Three  = One
  append Three One   = Zero
  append Three Two   = One
  append Three Three = Two

instance monMod4 :: Monoid Mod4 where
  mempty = Zero

-- 9.26
class Semigroup a <= Group a where
  ginverse :: a -> a

-- 9.28
instance gMod4 :: Group Mod4 where
  ginverse Zero  = Zero
  ginverse One   = Three
  ginverse Two   = Two
  ginverse Three = One
