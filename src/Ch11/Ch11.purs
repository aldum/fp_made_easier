module Ch11.Ch11 where

import Data.List (List(..), (:), foldl)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, show, ($))
import Undefined (undefined)


test :: Effect Unit
test = do
  log $ show $ reverse (10 : 20 : 30 : Nil) -- Prints (30 : 20 : 10 : Nil)

reverse :: List ~> List
reverse =
  let
    folder = \rl x -> x : rl
    starter = Nil
  in
    foldl folder starter
