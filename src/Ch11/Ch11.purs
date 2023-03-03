module Ch11.Ch11 where

import Data.Foldable (class Foldable, foldl, foldr)
import Data.List (List(..), singleton, (:))
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Ord (class Ord, (<), (>))
import Data.Semigroup (class Semigroup)
import Data.Semiring (class Semiring, zero)
import Effect (Effect)
import Effect.Console (logShow)
import Prelude (type (~>), Unit, discard, mempty, negate, otherwise, ($), (+), (-))
import Undefined (undefined)

test :: Effect Unit
test = do
  -- ❷ Prints (5 : -1 : 14 : 99 : Nil).
  logShow $ sum $ toList exTree

  logShow $ sumF exTree
  logShow $ foldr (+) zero exTree
  logShow $ foldl (-) zero exTree
  logShow $ foldr (-) zero exTree

-- 11.1
reverse :: List ~> List
reverse =
  let
    folder :: ∀ a. List a -> a -> List a
    folder = \rl x -> x : rl
    starter = Nil
  in
    foldl folder starter

-- 11.3
max :: ∀ a. (Ord a) => a -> a -> a
max x y | x < y = y
max x _ = x

maxB :: ∀ a. Ord a => a -> a -> a
maxB x y
  | x > y = x
  | otherwise = y

-- 11.6
findMax' :: ∀ a. Ord a => a -> List a -> a
findMax' acc Nil = acc
findMax' acc (x : xs) = findMax' (max acc x) xs

-- 11.8
findMax'' :: ∀ a. Ord a => List a -> Maybe a
findMax'' = go Nothing
  where
  go macc Nil = macc
  go macc (x : xs) =
    case macc of
      Nothing -> go (Just x) xs
      Just m -> go (Just (max x m)) xs

findMaxB :: ∀ a. Ord a => List a -> Maybe a
findMaxB Nil = Nothing
findMaxB l@(first : _) = Just $ go first l
  where
  go mx Nil = mx
  go mx (x : xs) = go (max x mx) xs

-- 11.9
findMax :: ∀ a. Ord a => List a -> Maybe a
findMax = foldl op Nothing
  where
  op :: Maybe a -> a -> Maybe a
  op mx n =
    case mx of
      Nothing -> Just n
      Just m -> Just (max m n)

-- 11.11
findMaxFB :: ∀ a. Ord a => List a -> Maybe a
findMaxFB Nil = Nothing
findMaxFB l@(first : _) = Just $ foldl max first l

findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE (NonEmptyList (NonEmpty first l)) = foldl max first l

-- 11.13
findMaxNE' :: ∀ t14 a. Foldable t14 => Ord a => NonEmpty t14 a -> a
findMaxNE' = foldl1 max

findMaxNEb :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNEb (NonEmptyList ne) = foldl1 max ne

-- 11.15
foldl1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1 folder (NonEmpty first rest) = foldl folder first rest

foldl1' :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1' f (x :| xs) = foldl f x xs

-- 11.18
sum :: List Int -> Int
sum Nil = 0
sum l = go 0 l
  where
  go acc Nil = acc
  go acc (x : xs) = go (acc + x) xs

sum' :: List Int -> Int
sum' Nil = 0
sum' (x : xs) = x + sum xs

sum'' :: List Int -> Int
sum'' = go 0
  where
  go acc Nil = acc
  go acc (x : xs) = go (acc + x) xs

-- 11.20
sumF' :: List Int -> Int
sumF' = foldl (+) 0

-- sumF :: List Number -> Number
sumF ∷ ∀ f a. Foldable f => Semiring a => f a → a
sumF = foldl (+) zero

-- 11.25
data Tree a = Leaf a | Node (Tree a) (Tree a)

exTree :: Tree Int
exTree =
  Node
    ( Node
        (Leaf 5)
        ( Node
            (Leaf (-1))
            (Leaf 14)
        )
    )
    (Leaf 99)

instance foldTree :: Foldable Tree where
  -- foldr :: ∀ a b. (a -> b -> b) -> b -> f a -> b
  foldr op acc (Leaf v) = v `op` acc
  foldr op acc (Node lt rt) =
    foldr op left rt
    where
    left = foldr op acc lt

  -- foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldl op acc (Leaf v) = acc `op` v
  foldl op acc (Node lt rt) =
    foldl op left rt
    where
    left = (foldl op acc lt)

  -- foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
  foldMap m (Leaf v) = m v
  foldMap m t = foldl lift mempty t
    where
    lift acc tr = acc <> m tr

toList :: Tree ~> List
toList (Leaf l) = singleton l
toList (Node lt rt) = toList lt <> toList rt
