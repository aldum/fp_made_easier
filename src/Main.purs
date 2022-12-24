module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Undefined (undefined)

import Ch7.Ch7a as Ch7a

main :: Effect Unit
main = Ch7a.test
