module Ch7.Ch7b where

import Prelude

import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log "placeholder"

class ToCSV a where
  toCSV :: a -> String
