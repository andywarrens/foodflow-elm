module Util exposing (tupleMap, stylesheet, stylesheetcdn)

import Html exposing (node)
import Html.Attributes exposing (attribute)

-- Functional utils
----------------------------------------
{-| Map over the tuple with two functions, one for each element.  -}
mapEach : (a -> a_) -> (b -> b_) -> (a, b) -> (a_, b_)
mapEach f g (a, b) = (f a, g b)

tupleMap : (a -> a_) -> (a, a) -> (a_, a_)
tupleMap f = mapEach f f


-- CSS
----------------------------------------
stylesheet url =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href" url 
            ]
        children = []
    in 
        node tag attrs children

stylesheetcdn url integrity crossorigin =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href" url 
            , attribute "integrity" integrity 
            , attribute "crossorigin" crossorigin 
            ]
        children = []
    in 
        node tag attrs children
