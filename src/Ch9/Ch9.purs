module Ch9.Ch9 where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Show (show, class Show)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, ($))

test :: Effect Unit
test = do
  log $ show $ ATrue <> ATrue -- ❶ Prints ATrue
  log $ show $ ATrue <> AFalse -- ❷ Prints AFalse
  log $ show $ AFalse <> AFalse -- ❸ Prints AFalse

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
