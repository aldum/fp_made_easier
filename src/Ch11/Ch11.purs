module Ch11.Ch11 where

import Data.Foldable (class Foldable)
import Data.List (List(..), (:), foldl)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Ord (class Ord, (<), (>))
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, otherwise, show, ($))
import Undefined (undefined)


test :: Effect Unit
test = do
  log $ show $ findMaxNEb (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil)) -- ❶ Prints 311.
  log $ show $ findMaxNEb (NonEmptyList $ "a" :| ("bbb" : "c" : Nil)) -- ❷ Prints "c"

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
maxB x y | x > y = x
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
        Just  m -> go (Just (max x m)) xs

findMaxB :: ∀ a. Ord a => List a -> Maybe a
findMaxB Nil = Nothing
findMaxB l@(first : _) = Just $ go first l where
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
        Just  m -> Just (max m n)

-- 11.11
findMaxFB :: ∀ a. Ord a => List a -> Maybe a
findMaxFB Nil = Nothing
findMaxFB l@(first : _) = Just $ foldl max first l

findMaxNE :: ∀ a. Ord a => NonEmptyList a ->  a
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
