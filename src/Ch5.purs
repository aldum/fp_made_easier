module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, negate, otherwise, show, (+), (-), (==), (/=), (<))

test :: Effect Unit
test = do
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int) -- ❷ Prints Nothing.
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil) -- ❹ Prints (Just 5)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil) -- ❺ Prints Nothing.
  log $ show $ findLastIndex (_ /= 10) (11 : 12 : Nil)
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

-- 5.20 --
uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head : x , tail : xs }

-- 5.21 --
index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index _ i | i < 0 = Nothing
index (x : _) 0 = Just x
index (_ : xs) i = index xs (i - 1)

indexB :: ∀ a. List a -> Int -> Maybe a
indexB Nil _ = Nothing
indexB  l i = go l i where
  go Nil _ = Nothing
  go (x : xs) ci | ci == 0  = Just x
                 | otherwise = go xs (ci - 1)
-- 5.22 --
infixl 8 index as !!

-- 5.23 --
findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex p l = go 0 l where
  go :: Int -> List a -> Maybe Int
  go _ Nil = Nothing
  -- go i (x : xs) = if p x then Just i else go (i + 1) xs
  go i (x : xs) | p x = Just i
                | otherwise = go (i + 1) xs

-- 5.24 --
findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex p l = go Nothing 0 l where
  go :: Maybe Int -> Int -> List a -> Maybe Int
  go mi _ Nil = mi
  go mi i (x : xs) | p x       = go (Just i) (i + 1) xs
                   | otherwise = go mi (i + 1) xs
