module Step exposing (Step, addStep, defaultStep, toForm, width)

import Ingredient exposing (..)
import Util exposing (calculateMove, size, initialBackground)

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

toForm : Step -> Form
toForm { ingredients, action } =
    let
      tileSize = toFloat size
      transformIngredient = Ingredient.toForm >> Collage.scale 0.8 >> Collage.move (-0.2*tileSize, 0.2*tileSize)
      ingredientForms = 
      (movedIngredients, _) = List.map transformIngredient ingredients
         |> List.foldr (\ingr (xs, i) -> ( (moveX (i*tileSize) ingr) :: xs, i+1 ) )
            ([], 0) ingredientForms
      text = fromString action |> leftAligned 
      textWidth = Element.widthOf text |> toFloat
      movedText = Collage.toForm text |> move (0.5*(textWidth-tileSize), -0.3*tileSize)
    in
      group (movedText :: movedIngredients)

addStep : (Float, Float) -> Step -> Form -> Form
addStep (col, row) step board =
    let
        (dx, dy) = calculateMove(col, row)
        blockForm = toForm step |> move (dx, dy)
    in group [board, blockForm]
