module Step exposing (Step, defaultStep, toForm, width)

import Ingredient exposing (..)

import Collage exposing (Form, collage, move, group, outlined, solid, rect)
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


calculateLocations : Int -> List Float
calculateLocations nIngredients =
  let
      cumulativeSum = List.scanl (+) 0
      iota n = List.repeat (n - 1) 1 |> cumulativeSum
      translate a = toFloat (a * Ingredient.size)
  in iota nIngredients |> List.map translate
      
width : Step -> Int
width { ingredients } = List.length ingredients

toForm : Step -> Form
toForm { ingredients, action } =
    let
      ingredientForms = List.map Ingredient.toForm ingredients
      locations = calculateLocations (List.length ingredients)
      movedIngredients = List.map2 Collage.moveX locations ingredientForms
      halfSize = (toFloat Ingredient.size) / 2
      text = fromString action |> leftAligned 
      textWidth = Element.widthOf text |> toFloat
      movedText = Collage.toForm text |> move (textWidth*0.5-halfSize, -halfSize*1.2)
    in
      group <| movedIngredients ++ [movedText]

addStep : (Float, Float) -> Step -> Form -> Form
addStep (col, row) step board =
    let
        (dx, dy) = Ingredient.calculateMove(col, row)
        blockForm = toForm step |> move (dx, dy)
    in Collage.group [board, blockForm]

main = 
  let screen = addStep (0, 1) defaultStep Ingredient.initialBackground
      bigtext = addStep (0, 0) { ingredients = [ zongedroogdtomaten, littlegem ], action = "Snijd in stukjes en reepjes" } Ingredient.initialBackground
      viewportx = round (Ingredient.cols*(toFloat size))
      viewporty = round (Ingredient.rows*(toFloat size))
      viewportx_s = round <| 0.5 * (toFloat viewportx)
      viewporty_s = round <| 0.5 * (toFloat viewporty)
  in toHtml <| flow right 
      [ collage viewportx_s viewporty_s [Collage.scale 0.5 bigtext]
      , collage viewportx_s viewporty_s [Collage.scale 0.5 screen]
      , show (calculateLocations 3) ]
