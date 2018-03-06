module Step exposing (Step, defaultStep)

import Ingredient exposing (..)

import Collage exposing (Form, collage)
import Element exposing (toHtml, show, flow, right, down)


type alias Step = 
    { ingredients : List Ingredient
    , action : String }

type alias ViewStep = 
    { step : Step
    , location : (Float, Float) }

defaultStep : Step
defaultStep = 
    { ingredients = Ingredient.ingredients
    , action = "Mix together" }

defaultViewStep : ViewStep
defaultStep =
    { step = defaultStep
    , location = ( (toFloat Ingredient.size) / 2,
                 , (toFloat Ingredient.size) / 2) }

toForm : ViewStep -> (List Int, Form)
toForm { step, location } =
    let
      text = "<action>"
      ingredientForms = List.map Ingredient.toForm step.ingredients
      cumulativeSum = List.scanl (+) 0
      iota n = List.repeat (n - 1) 1 |> cumulativeSum
      translate a = a * Ingredient.size
      locations = List.map translate <| iota (List.length step.ingredients)
      move dx = Collage.moveX (toFloat dx)
    in
      (locations, Collage.group (List.map2 move locations ingredientForms))

moveToOrigin : ViewStep -> ViewStep
moveToOrigin : form
  move (

main = 
    let
        (locations, stepForm) = toForm defaultStep
    in
        toHtml <| flow down 
          [ collage 800 400 [stepForm]
          , show locations ]
