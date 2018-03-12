module MergeStep exposing (MergeStep, defaultMergeStep)

import Ingredient exposing (..)
import Step exposing (..)

import Collage exposing (Form, collage, move, group)
import Element exposing (toHtml, show, flow, right, leftAligned)
import Text exposing (fromString)


type alias MergeStep = 
    { steps : List Step
    , action : String }

defaultMergeStep : MergeStep
defaultMergeStep = 
    { steps = 
        [ Step.defaultStep
        , Step.defaultStep
        , { ingredients =
              [ { name = "Spaghetti", qty = 500, unit = "gram", img = "img/spaghetti.jpg" }
              , { name = "Water", qty = 2, unit = "litre", img = "img/water.jpg" } ]
          , action = "Boil" } ]
    , action = "Drain spaghetti and put everything together on a plate" }

calculateStepLocations : Int -> List Float
calculateStepLocations nSteps =
  let cumulativeSum = List.scanl (+) 0
      iota n = List.repeat (n - 1) 1 |> cumulativeSum
      translate a = List.foldl (*) -1 [a, (toFloat Ingredient.size),  1.2] -- leave some room for "action"-text
  in iota nSteps |> List.map translate
      
createOffsetText : Int -> Text.Text -> Form
createOffsetText nSteps textObj =
    let text = leftAligned textObj
        halfSize = (toFloat Ingredient.size) / 2
        dx = (Element.widthOf text |> toFloat) / 2 - halfSize
        dy = (toFloat (-1 * nSteps * Ingredient.size)) * 1.2 + halfSize * 0.8
    in Collage.toForm text |> move (dx, dy) 


toForm : MergeStep -> Form
toForm { steps, action } =
    let
      stepForms = List.map Step.toForm steps
      locations = calculateStepLocations (List.length steps)
      movedSteps = List.map2 Collage.moveY locations stepForms
      text = createOffsetText (List.length steps) (fromString action)
    in
      group <| movedSteps ++ [text]

addMergeStep : (Float, Float) -> MergeStep -> Form -> Form
addMergeStep (col, row) mergeStep board =
    let
        (dx, dy) = Ingredient.calculateMove(col, row)
        blockForm = toForm mergeStep |> move (dx, dy)
    in group [board, blockForm]


main =
  let screen = addMergeStep (0, 0) defaultMergeStep Ingredient.initialBackground
  in toHtml <| flow right 
      [ collage (round (Ingredient.cols*(toFloat size)))
                (round (Ingredient.rows*(toFloat size))) 
                [screen]
      ]

