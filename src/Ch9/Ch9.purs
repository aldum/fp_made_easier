module Ch9.Ch9 where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit)

test :: Effect Unit
test = do
  log "placeholder"

class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>
