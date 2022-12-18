module Ch5 where

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show)

test :: Effect Unit
test = do
  log $ show $ snoc (1 : 2 : Nil) 3
  log "🍝"

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

-- 5.11 --
singleton :: ∀ a. a -> List a
singleton a = a : Nil

-- 5.12 --
null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

-- 5.13 --
snoc :: ∀ a. List a -> a -> List a
snoc Nil n = singleton n
snoc (x : xs) n = x : snoc xs n

-- ----------------
flip' :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
flip' f = \b a -> f a b

flip'' :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
flip'' f b = \a -> f a b
