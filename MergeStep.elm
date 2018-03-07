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
        , { ingredients =
              [ { name = "Spaghetti", qty = 500, unit = "gram", img = "img/spaghetti.jpg" }
              , { name = "Water", qty = 2, unit = "litre", img = "img/water.jpg" } ]
          , action = "Boil" } ]
    , action = "Drain spaghetti and put everything together on a plate" }

calculateStepLocations : Int -> List Float
calculateStepLocations nSteps =
  let cumulativeSum = List.scanl (+) 0
      iota n = List.repeat (n - 1) 1 |> cumulativeSum
      translate a = List.foldl (*) 1 [a, (toFloat Ingredient.size),  1.2] -- leave some room for "action"-text
  in iota nSteps |> List.map translate
      
calculateTextOffset : Int -> (Float, Float)
calculateTextOffset nSteps =
    let halfSize = (toFloat Ingredient.size) / 2
        dx = -0.1 * (toFloat Ingredient.size)
        dy = -halfSize - (toFloat (nSteps * Ingredient.size)) * 1.2
    in (dx, dy)


toForm : MergeStep -> Form
toForm { steps, action } =
    let
      stepForms = List.map Step.toForm steps
      locations = calculateStepLocations (List.length steps)
      movedSteps = List.map2 Collage.moveY locations stepForms
      textOffset = calculateTextOffset (List.length steps)
      text = fromString action |> leftAligned 
        |> Collage.toForm |> move textOffset
    in
      group <| movedSteps ++ [text]




main =
  let screen = toForm defaultMergeStep
  in toHtml <| flow right 
      [ collage (round (Ingredient.cols*(toFloat size)))
                (round (Ingredient.rows*(toFloat size))) 
                [screen]
      , show <| calculateTextOffset (List.length defaultMergeStep.steps) ]

