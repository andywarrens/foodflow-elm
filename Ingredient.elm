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

addIngredient : (Float, Float) -> Ingredient -> Form -> Form
addIngredient (x, y) ingredient board =
    let
        ingredientSize = toFloat size
        offsetX = -((x - 1) * ingredientSize)/2
        offsetY = -((y - 1) * ingredientSize)/2
        dx = x * ingredientSize + offsetX
        dy = y * ingredientSize + offsetY
        blockForm = toForm ingredient |> move (dx, dy)
    in group [board, blockForm]
        
size : Int
size = 100

initialBackground : Form
initialBackground =
  let shape = Collage.rect 600 400
      border = outlined (solid Color.black) shape
  in group [filled Color.lightGray shape, border]

---------------
---------------
---------------

viewportSize : Int
viewportSize = 240

ingredientSize : Float
ingredientSize = (toFloat viewportSize) / 2

viewport : Collage.Form
viewport = square (toFloat viewportSize) |> outlined { defaultLine | width = 3 } 

main = 
  let screen = addIngredient (100, 100) (Ingredient "Tomato" 4 "piece" "img/tomato.jpg") initialBackground
  in toHtml <| collage 800 600 [screen]

{-
  21/2:
  The hard thing to do here is the translation of the cropped images.
  When adding an image Element to a Collage, the middle of the image will be placed on the middle of the Collage.
  So when we want to move the image, this becomes a problem with e.g. the tomate in the upper left corner:
   - full tomato: moving the middle with moveToOrigin will draw tomato img edges nicely on top of the collage edges
   - cropped tomate: because this image is smaller, the img and collage edges
   will not overlap now even though the middle of both images will overlap

    TODO: I think in Tetris they had a way of handling this situation
-}
-- main = 
--     let 
--       tomatoImgsize = 2400
--       halfTomatoSize = round ((toFloat tomatoImgsize) / 2)
--       scale = ingredientSize / (toFloat tomatoImgsize)
--       moveToOrigin = move (-ingredientSize/2, ingredientSize/2) -- moves left-top-corner of img to left-top-corner of grid
--       tomatoScale = Collage.scale scale <| Collage.toForm <| Element.image tomatoImgsize tomatoImgsize "img/tomato.jpg"
--       tomatoMoved = moveToOrigin <| tomatoScale
--       tomatoUL = Collage.toForm <| Element.croppedImage (0,0) halfTomatoSize halfTomatoSize "img/tomato.jpg"
--       tomatoULScale = Collage.scale scale <| tomatoUL
--       cropDx = 3/4*ingredientSize
--       tomatoULMove = move (-cropDx, cropDx) <| tomatoULScale
--       tomatoBR = move (ingredientSize, -ingredientSize) <| moveToOrigin <| Collage.scale scale
--         <| Collage.toForm <| Element.croppedImage (halfTomatoSize, halfTomatoSize) halfTomatoSize halfTomatoSize "img/tomato.jpg"
--     in
--       toHtml <| flow right [
--             collage viewportSize viewportSize [ viewport, tomatoUL           ]
--            ,collage viewportSize viewportSize [ viewport, tomatoULScale      ]
--            ,collage viewportSize viewportSize [ viewport, tomatoULMove       ]
--            ,collage viewportSize viewportSize [ viewport, tomatoScale        ]
--            ,collage viewportSize viewportSize [ viewport, tomatoMoved        ]
--           ,show scale ]

