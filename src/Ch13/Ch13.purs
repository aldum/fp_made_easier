module Ch13 where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, ($), (/), (<$>))

test :: Effect Unit
test = do
  log $ show $ (_ / 2) <$> Just 10 -- ❶ Prints (Just 5).
  log $ show $ (_ / 2) <$> Nothing -- ❷ Prints Nothing.
