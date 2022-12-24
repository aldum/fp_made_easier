module Ch7.Ch7a where


-- data Maybe a = Nothing | Just a
data Option a = None | Some a -- let's Scala this up a bit

someInt :: Option Int
someInt = Some 1

none :: ∀ a. Option a
none = None
