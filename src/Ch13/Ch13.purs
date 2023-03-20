module Ch13 where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, ($), (/))
import Undefined (undefined)

test :: Effect Unit
test = do
  log $ show $ (_ / 2) <$> Just 10 -- ❶ Prints (Just 5).
  log $ show $ (_ / 2) <$> Nothing -- ❷ Prints Nothing.

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

instance mf :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just a) = Just (f a)
