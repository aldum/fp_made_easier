module Ch5 where

import Prelude (Unit, discard, show)
import Data.List (List(..), (:))

import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log (show (flip const 1 2))
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  -- log $ show (flip' const 1 2)
  -- log $ show (flip'' const 1 2)
  log "test 🍝"

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f a b = f b a

const :: ∀ a b. a -> b -> a
const a _ = a

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped x f = f x

applyFlipped' :: ∀ a b. a -> (a -> b) -> b
applyFlipped' = flip apply

-- infixl 0 applyFlipped as #
infixl 1 applyFlipped as #


-- ----------------

flip' :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
flip' f = \b a -> f a b

flip'' :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
flip'' f b = \a -> f a b
