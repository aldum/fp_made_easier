module Ch6.Multiple where


import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe)
import Data.String.CodeUnits as String

-- 6.12
class Decapitate collection element where
  decapitate :: collection -> Maybe {head :: element, tail :: collection}
instance decapitateList :: Decapitate (List a) a where
  decapitate = List.uncons
instance decapitateString :: Decapitate String Char where
  decapitate = String.uncons
