module Util exposing (..)

import Html exposing (node)
import Html.Attributes exposing (attribute)

import Collage exposing (Form, rect, outlined, group, filled, solid)
import Color exposing (black, lightGray)

-- Layout and types
----------------------------------------

size : Int
size = 100

cols : Float
cols = 8

rows : Float
rows = 6

type alias Url = String

calculateMove : (Float, Float) -> (Float, Float)
calculateMove (col, row) =
    let offsetX = (cols-1)/2*(toFloat size)         -- e.g. with 5 columns wide, the origin starts at 2.5, so do (-2)
        offsetY = (rows-1)/2*(toFloat size)
        dx = col * (toFloat size) - offsetX
        dy = -row * (toFloat size) + offsetY
    in (dx, dy)

initialBackground : Form
initialBackground = 
  let width = cols*(toFloat size) 
      height = rows*(toFloat size)
      shape = rect width height
  in group [filled lightGray shape
           ,createOutline width height]

createOutline : Float -> Float -> Form
createOutline width height =
  let shape = rect width height
  in outlined (solid black) shape


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
