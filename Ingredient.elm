module Ingredient exposing (Ingredient, ingredients, size, toForm)

import Html exposing (text, div, p)
import Html.Attributes exposing (class)
import Element exposing (toHtml, show, flow, right, down)
import Collage exposing (scale, move, collage, Form, group, groupTransform, square, outlined, filled, solid, defaultLine)
import Color exposing (lightGray)

import List exposing (singleton, map)


type alias Ingredient = 
    { name : String
    , qty  : Float
    , unit : String
    , img  : String }

ingredients : List Ingredient
ingredients = 
    [ Ingredient "Tomato" 4 "piece" "img/tomato.jpg"
    , Ingredient "Onion" 1 "piece" "img/onion.jpg"
    , Ingredient "Turkey" 220 "gram" "img/turkey.jpg" ]

toForm : Ingredient -> Collage.Form
toForm { img } =
    Element.image size size img |> Collage.toForm

calculateMove : (Float, Float) -> (Float, Float)
calculateMove (col, row) =
    let offsetX = (cols-1)/2*(toFloat size)         -- e.g. with 5 columns wide, the origin starts at 2.5, so do (-2)
        offsetY = (rows-1)/2*(toFloat size)
        dx = col * (toFloat size) - offsetX
        dy = -row * (toFloat size) + offsetY
    in (dx, dy)

addIngredient : (Float, Float) -> Ingredient -> Form -> Form
addIngredient (col, row) ingredient board =
    let
        (dx, dy) = calculateMove(col, row)
        blockForm = toForm ingredient |> move (dx, dy)
        --blockForm = toForm ingredient |> move (-offsetX, offsetY)
    in group [board, blockForm]
        
size : Int
size = 100

cols : Float
cols = 8

rows : Float
rows = 6

initialBackground : Form
initialBackground =
  let shape = Collage.rect (cols*(toFloat size)) (rows*(toFloat size))
      border = outlined (solid Color.black) shape
  in group [filled Color.lightGray shape, border]

---------------
---------------
---------------

main = 
  let tomato = Ingredient "Tomato" 4 "piece" "img/tomato.jpg"
      screen = addIngredient (cols-1, rows-1) tomato initialBackground
  in toHtml <| flow right 
      [ collage (round (cols*(toFloat size))) (round (rows*(toFloat size))) [screen]
      , show <| calculateMove (0, 2) ]
