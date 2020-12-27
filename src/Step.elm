module Step exposing (Step, addStep, defaultStep, defaultStep2, toForm, width)

import Ingredient exposing (..)
import Util exposing (calculateMove, cols, rows, size, initialBackground)

import Collage exposing (group, circle, rectangle, filled)
import Collage exposing (defaultLineStyle, outlined, solid, uniform)
import Collage exposing (shift, shiftX, rendered)

import Collage.Text as Text
import Collage.Text exposing (fromString)
import Collage.Layout exposing (align, left)

import Color exposing (Color)

type alias Step = 
    { ingredients : List Ingredient
    , action : String }

defaultStep : Step
defaultStep = 
    { ingredients = Ingredient.ingredients
    , action = "Mix together" }

defaultStep2 : Step
defaultStep2 = 
    { ingredients =
        [ { name = "Spaghetti", qty = 500, unit = "gram", img = "img/spaghetti.png" }
        , { name = "Water", qty = 2, unit = "litre", img = "img/water.png" } ]
    , action = "Boil" }

      
width : Step -> Int
width { ingredients } = List.length ingredients

toForm : Step -> (Collage.Collage msg, Float)
toForm { ingredients, action } =
    let
      tileSize = toFloat size
      scale = 0.8
      scaleMargin = 0.5*(1-scale) * tileSize
      transformIngredient = Ingredient.toForm 
        >> Collage.scale 0.8 
        >> Collage.shift (-scaleMargin, scaleMargin)
      translateIngredient ingr (xs, i) = ( (shiftX (i*scale*tileSize) ingr) :: xs, i+1 ) 
      (shiftedIngredients, _) = ingredients
         |> List.map transformIngredient 
         |> List.foldl translateIngredient ([], 0)
      text      = fromString action 
        |> rendered
        |> align left
      textWidth = fromString action |> Text.width 
      ingrWidth = tileSize*(toFloat (List.length ingredients))
      stepwidth = max textWidth ingrWidth
      -- let's move the ingredients so that the centerpoint of the result Form
      -- is the middle ingredient
      movedText = text 
        |> shift (0.5*(textWidth-stepwidth), -0.42*tileSize)
      movedIngredients = shiftedIngredients |>
        group >> shiftX (0.5*(tileSize-stepwidth))
    in
      (group [movedText, movedIngredients] , stepwidth)

addStep : (Float, Float) -> Collage.Collage msg -> Step -> Collage.Collage msg
addStep (col, row) board step =
    let
        (dx, dy) = calculateMove(col, row)
        blockForm = toForm >> Tuple.first >> shift (dx, dy)
    in group [board, blockForm step]
