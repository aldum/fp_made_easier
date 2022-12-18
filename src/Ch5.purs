module Ch5 where

import Prelude (Unit, discard, show)

import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log "test 🍝"
