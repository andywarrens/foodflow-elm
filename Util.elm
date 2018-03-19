module Util exposing (tupleMap)

{-| Map over the tuple with two functions, one for each element.  -}
mapEach : (a -> a_) -> (b -> b_) -> (a, b) -> (a_, b_)
mapEach f g (a, b) = (f a, g b)

tupleMap : (a -> a_) -> (a, a) -> (a_, a_)
tupleMap f = mapEach f f
