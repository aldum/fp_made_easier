module Ch7.Ch7a where


-- data Maybe a = Just a | Nothing
data Option a = Some a | None -- let's Scala this up a bit

someInt :: Option Int
someInt = Some 1

none :: ∀ a. Option a
none = None
