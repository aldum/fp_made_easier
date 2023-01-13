module Ch11.Ch11 where

import Data.List (List(..), (:), foldl)
import Data.Ord (class Ord, (<))
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), discard, negate, Unit, show, ($))
import Undefined (undefined)


test :: Effect Unit
test = do
  log $ show $ max (-1) 99 -- ❶ Prints 99
  log $ show $ max "aa" "z" -- ❷ Prints "z"

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
