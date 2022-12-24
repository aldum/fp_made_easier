module Main where

import Prelude

import Ch6.Multiple (t, tu)
import Effect (Effect)
import Effect.Console (log)
import Undefined (undefined)

main :: Effect Unit
main = do
  log $ show $ t
  log $ show $ tu
