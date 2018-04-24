module Step exposing (Step, addStep, defaultStep, toForm, width)

import Ingredient exposing (..)
import Util exposing (calculateMove, cols, rows, size, initialBackground)

import Collage exposing (Form, collage, move, moveX, group, outlined, solid, rect)
import Element exposing (toHtml, show, flow, right, leftAligned)
import Text exposing (fromString)
import Color exposing (blue)


type alias Step = 
    { ingredients : List Ingredient
    , action : String }

defaultStep : Step
defaultStep = 
    { ingredients = Ingredient.ingredients
    , action = "Mix together" }
      
width : Step -> Int
width { ingredients } = List.length ingredients

toForm : Step -> (Form, Float)
toForm { ingredients, action } =
    let
      tileSize = toFloat size
      scale = 0.8
      scaleMargin = 0.5*(1-scale) * tileSize
      transformIngredient = Ingredient.toForm 
        >> Collage.scale 0.8 >> Collage.move (-scaleMargin, scaleMargin)
      (shiftedIngredients, _) = List.map transformIngredient ingredients
         |> List.foldr (\ingr (xs, i) -> ( (moveX (i*scale*tileSize) ingr) :: xs, i+1 ) )
            ([], 0)
      text      = action |> fromString >> leftAligned
      textWidth = text   |> Element.widthOf >> toFloat
      ingrWidth = tileSize*(toFloat (List.length ingredients))
      width = max textWidth ingrWidth
      -- let's move the ingredients so that the centerpoint of the result Form
      -- is the middle ingredient
      movedText = text |> 
        Collage.toForm >> move (0.5*(textWidth-width), -0.42*tileSize)
      movedIngredients = shiftedIngredients |>
        group >> moveX (0.5*(tileSize-width))
    in
      (group [movedText, movedIngredients]
      ,width)

addStep : (Float, Float) -> Form -> Step -> Form
addStep (col, row) board step =
    let
        (dx, dy) = calculateMove(col, row)
        blockForm = toForm >> Tuple.first >> move (dx, dy)
    in group [board, blockForm step]
