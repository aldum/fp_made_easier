module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, (+), (==))

test :: Effect Unit
test = do
  log $ show $ init (Nil :: List Unit)
  log $ show $ init $ 1 : Nil
  log $ show $ init $ 1 : 2 : Nil
  log $ show $ init $ 1 : 2 : 3 : Nil

  log $ show $ init' (Nil :: List Unit)
  log $ show $ init' $ 1 : Nil
  log $ show $ init' $ 1 : 2 : Nil
  log $ show $ init' $ 1 : 2 : 3 : Nil
  log "🍝"

-- 5.4 --

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f a b = f b a

flip' :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
flip' f = \b a -> f a b

flip'' :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
flip'' f b = \a -> f a b

-- 5.5 --

const :: ∀ a b. a -> b -> a
const a _ = a

-- 5.7 --

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

-- 5.8 --

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

-- 5.14 --
length :: ∀ a. List a -> Int
length Nil = 0
length (_ : xs) = 1 + length xs

-- 5.15 --
length' :: ∀ a. List a -> Int
length' l = go 0 l where
  go :: Int -> List a -> Int
  go acc Nil      = acc
  go acc (_ : xs) = go (acc + 1) xs

lengthTail :: ∀ a. Int -> List a -> Int
lengthTail acc Nil = acc
lengthTail acc (_ : xs) = lengthTail (acc + 1) xs

-- 5.16 --
head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

-- 5.17 --
tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

tail' :: ∀ a. List a -> List a
tail' Nil = Nil
tail' (_ : xs) = xs

-- 5.18 --
last :: ∀ a. List a -> Maybe a
last      Nil  = Nothing
last (x : Nil) = Just (x)
last (_ : xs)  = last xs

-- from the book
lastB :: ∀ a. List a -> Maybe a
lastB      Nil = Nothing
lastB (_ : xs) = if length xs == 1 then head xs else last xs

last' :: ∀ a. List a -> Maybe a
last' Nil                       = Nothing
last' (_ : xs) | length xs == 1 = head xs
last' (_ : xs)                  = last xs

-- 5.19 --
init :: ∀ a. List a -> Maybe (List a)
init Nil  = Nothing
init l    = go Nil l where
  go :: List a -> List a -> Maybe (List a)
  go _ Nil         = Nothing
  go acc (_ : Nil) = Just acc
  go acc (y : ys)  = go (snoc acc y) ys

init' :: ∀ a. List a -> Maybe (List a)
init' Nil  = Nothing
init' l    = Just $ go l where
  go :: List a -> List a
  go Nil = Nil
  go (_ : Nil) = Nil
  go (y : ys)  = y : go ys
