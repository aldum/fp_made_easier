module Ch11.Ch11 where

import Data.List (List(..), (:), foldl)
import Data.Ord (class Ord, (<), (>))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, otherwise, show, ($))
import Undefined (undefined)


test :: Effect Unit
test = do
  log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil) -- ❶ Prints 311.
  log $ show $ findMax ("a" : "bbb" : "c" : Nil) -- ❷ Prints "c".

-- 11.1
reverse :: List ~> List
reverse =
  let
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
findMax :: ∀ a. Ord a => List a -> Maybe a
findMax = go Nothing
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
