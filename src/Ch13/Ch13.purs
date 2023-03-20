module Ch13 where

import Data.Generic.Rep (class Generic)
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, ($), (/))
import Undefined (undefined)

-- 13.1

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance mf :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just a) = Just (f a)

-- 13.3

data Either l r = Left l | Right r

derive instance genericEither :: Generic (Either a b) _
-- instance showEither :: (Show l, Show r) => Show (Either l r) where
--   show = genericShow

instance showEither :: (Show l, Show r) => Show (Either l r) where
  show (Left l) = "(Left " <> show l <> ")"
  show (Right r) = "(Right " <> show r <> ")"

instance ef :: Functor (Either l) where
  -- map _ left = left
  map _ (Left e) = Left e
  map f (Right v) = Right $ f v

-- 13.5

data Tuple a b = Tuple a b

derive instance genericTuple :: Generic (Tuple a b) _
instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show = genericShow

instance tf :: Functor (Tuple a) where
  map f (Tuple a b) = Tuple a (f b)

test :: Effect Unit
test = do
  log $ show $ (_ / 2) <$> Tuple 10 20 -- ❶ Prints (Tuple 10 10)
