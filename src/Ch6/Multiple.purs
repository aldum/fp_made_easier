module Ch6.Multiple where


import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as StringUnicode
import Data.String.CodePoints (CodePoint, codePointFromChar)
import Data.String.CodeUnits as String
import Type.Proxy (Proxy(..))

-- 6.12
class Decapitate collection element where
  decapitate :: collection -> Maybe {head :: element, tail :: collection}
instance decapitateList :: Decapitate (List a) a where
  decapitate = List.uncons
instance decapitateString :: Decapitate String Char where
  decapitate = String.uncons
-- 6.13
instance decapitateStringUnicode :: Decapitate String CodePoint where
  decapitate = StringUnicode.uncons

-- genericTail
--   :: ∀ collection element
--   . Decapitate collection element
--   => collection
--   -> Maybe collection
-- genericTail xs = case decapitate xs of -- COMPILER ERROR!!
--   Just { tail } -> Just tail
--   Nothing -> Nothing

-- hacky
genericTailH
  :: ∀ collection element
  . Decapitate collection element
  => element
  -> collection
  -> Maybe collection
genericTailH _ xs =
  case (decapitate xs :: Maybe {head :: element, tail :: collection}) of
    Just { tail } -> Just tail
    Nothing -> Nothing

tH :: Maybe String
tH = genericTailH 'c' "abc"

tuH :: Maybe String
tuH = genericTailH (codePointFromChar 'c') "abc"


genericTail
  :: ∀ collection element
  . Decapitate collection element
  => Proxy element
  -> collection
  -> Maybe collection
genericTail _ xs = case (decapitate xs :: Maybe {head :: element, tail ::
collection}) of
  Just { tail } -> Just tail
  Nothing -> Nothing

t :: Maybe String
t = genericTail (Proxy :: Proxy Char) "abc"

tu :: Maybe String
tu = genericTail (Proxy :: Proxy CodePoint) "abc"
