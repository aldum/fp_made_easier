module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, Void, discard, max, negate, otherwise, show, (+), (-), (<), (<<<), (<=), (==), (>), (>=), (>>>))
import Undefined (undefined)

test :: Effect Unit
test = do
  log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil) -- ❶ Prints (4 : 5 : 6 : Nil)
  log $ show $ takeEnd 10 (1 : Nil) -- ❷ Prints (1 : Nil)
  log $ show $ takeEnd 1 (1 : 2 : 3 : 4 : Nil) -- Prints (4 : Nil)
  log $ show $ takeEnd 5 (1 : 2 : Nil) -- Prints (1 : 2 : Nil)
  log "🍝"
  log $ show $ takeEndB' 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil) -- ❶ Prints (4 : 5 : 6 : Nil)
  log $ show $ takeEndB' 10 (1 : Nil) -- ❷ Prints (1 : Nil)
  log $ show $ takeEndB' 1 (1 : 2 : 3 : 4 : Nil) -- Prints (4 : Nil)
  log $ show $ takeEndB' 5 (1 : 2 : Nil) -- Prints (1 : 2 : Nil)
  log "🍝"


void :: ∀ a . a -> Void
void = undefined

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
index Nil _       = Nothing
index _ i | i < 0 = Nothing
index (x : _) 0   = Just x
index (_ : xs) i  = index xs (i - 1)

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

-- 5.26 --
-- reverse :: ∀ a. List a -> List a
reverse :: List ~> List
reverse Nil      = Nil
reverse (x : xs) = snoc (reverse xs) x

reverse' :: ∀ a. List a -> List a
reverse' l = go Nil l where
  go :: List a -> List a -> List a
  go acc Nil = acc
  go acc (x : xs) = go (x : acc) xs

-- 5.27 --
concat2 :: ∀ a. List a -> List a -> List a
concat2 Nil l = l
concat2 l Nil = l
concat2 l1 l2 = go l1 l2 where
  go :: List a -> List a -> List a
  go acc Nil = acc
  go acc (x : xs) = go (snoc acc x) xs

concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat ll = go Nil ll where
  go :: List a -> List (List a) -> List a
  go acc Nil = acc
  go acc (l : ls) = go (concat2 acc l) ls

concat' :: ∀ a. List (List a) -> List a
concat' Nil = Nil
-- concat ??? = x : concat ??? -- from the book
concat' (l : Nil) = l
concat' (Nil : ls) = concat (ls)
concat' ((x : xs) : ls) = x : concat (xs : ls)

-- 5.28 --
filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter p l = go l where
  go Nil                  = Nil
  go (x : xs) | p x       = x : go xs
              | otherwise = go xs

-- 5.29 --
filterTail :: ∀ a. (a -> Boolean) -> List a -> List a
-- filterTail p l = go Nil l where
filterTail p = go Nil where
  go :: List a -> List a -> List a
  go acc Nil = reverse acc
  go acc (x : xs) = if p x then go (x: acc) xs else go acc xs

filterTail' :: ∀ a. (a -> Boolean) -> List a -> List a
filterTail' p = reverse <<< go Nil where
  go nl Nil = nl
  go nl (x : xs) = if p x then go (x : nl) xs else go nl xs

-- 5.31 --
catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil      = Nil
catMaybes (m : ms) = case m of
  Nothing -> catMaybes ms
  Just x -> x : catMaybes ms

-- 5.32 --
sign :: Int -> Int
sign a = if a > 0 then 1 else -1

range :: Int -> Int -> List Int
range s e = go Nil s e where
  d = sign (e - s)
  go :: List Int -> Int -> Int -> List Int
  go acc si ei | si == ei  = reverse (si : acc)
               | otherwise = go (si : acc) (si + d) ei

-- 5.33 --
range' :: Int -> Int -> List Int
range' s e = go Nil s where
  d = sign (e - s)
  go :: List Int -> Int -> List Int
  go acc i | i == e  = reverse (i : acc)
           | otherwise = go (i : acc) (i + d)

range'' :: Int -> Int -> List Int
range'' s e = go Nil e s where
  d = if s < e then (-1) else 1
  go :: List Int -> Int -> Int -> List Int
  go acc s' e' | s' == e'  = (s' : acc)
               | otherwise = go (s' : acc) (s' + d) e'

-- 5.34 --
take :: ∀ a. Int -> List a -> List a
take n l = go Nil (max 0 n) l where
  go :: List a -> Int -> List a -> List a
  go acc 0 _         = reverse acc
  go acc _ Nil       = reverse acc
  go acc n' (x : xs) = go (x : acc) (n' - 1) xs

take' :: ∀ a. Int -> List a -> List a
take' = go Nil where
  go :: List a -> Int -> List a -> List a
  go acc 0 _             = reverse acc
  go acc neg _ | neg < 0 = reverse acc
  go acc _ Nil           = reverse acc
  go acc n (x : xs)      = go (x : acc) (n - 1) xs


-- 5.35 --
drop :: ∀ a. Int -> List a -> List a
drop = go where
  go :: Int -> List a -> List a
  go 0 l        = l
  go _ Nil      = Nil
  go n (_ : xs) = go (n - 1) xs

dropB :: ∀ a. Int -> List a -> List a
dropB 0 l        = l
dropB _ Nil      = Nil
dropB n (_ : xs) = dropB (n - 1) xs

-- 5.36 --
takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile = go Nil where
  go :: List a -> (a -> Boolean) -> List a -> List a
  go acc _ Nil = reverse acc
  go acc p (x : xs) | p x       = go (x : acc) p xs
                    | otherwise = reverse acc

takeWhile' :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile' _ Nil                  = Nil
takeWhile' p (x : xs) | p x       = x : takeWhile' p xs
                      | otherwise = Nil

takeWhile'' :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile'' _ Nil            = Nil
takeWhile'' p (x : xs) | p x = x : takeWhile'' p xs
takeWhile'' _ _              = Nil

-- 5.37 --
dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile p (x: xs) | p x = dropWhile p xs
dropWhile _ l             = l

-- 5.38 --
takeEndB :: ∀ a. Int -> List a -> List a
takeEndB n l = drop (max 0 $ length l - n) l

takeEnd :: ∀ a. Int -> List a -> List a
takeEnd n _ | n <= 0 = Nil
takeEnd n l =
  let
    go :: List a -> Tuple Int (List a)
    go Nil      = Tuple 0 Nil
    go (x: Nil) = Tuple 1 $ singleton x
    go (x : xs) = Tuple (len + 1) l' where
      Tuple len li = go xs
      l' =  if len < n then x : li else li
  in
    snd $ go l

infixl 1 applyFlipped as |>

takeEndB' :: ∀ a. Int -> List a -> List a
takeEndB' n = go >>> snd where
  go Nil      = Tuple 0 Nil
  -- go (x : xs) = go xs
  --   |> \(Tuple c nl) -> Tuple (c + 1) $ if c < n then x : nl else nl
  go (x : xs) = go xs
    |> \tup@(Tuple c nl) -> if c < n then Tuple (c + 1) (x : nl) else tup

-- 5.40 --
-- 5.41 --
-- 5.42 --