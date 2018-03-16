module Recipe exposing (..)

import Step exposing (Step, defaultStep, toForm)
import Ingredient exposing (cols, rows, size, calculateMove)

import Html exposing (text, div, p)
import Html.Attributes exposing (class)
import Element exposing (toHtml, show, flow, right, down)
import Collage exposing (Form, move, collage, toForm, group, solid, square, outlined)
import Color exposing (red)

import List exposing (singleton, map)

defaultStep2 : Step
defaultStep2 = 
    { ingredients =
        [ { name = "Spaghetti", qty = 500, unit = "gram", img = "img/spaghetti.jpg" }
        , { name = "Water", qty = 2, unit = "litre", img = "img/water.jpg" } ]
    , action = "Boil" }

type RecipeList = Empty 
                | Node Step RecipeList          -- ofwel een node met een Step
                | Merge RecipeList RecipeList   -- ofwel een node die split

type alias Recipe = 
    { recipe   : RecipeList
    , name     : String
    , comments : String }

defaultRecipe : Recipe
defaultRecipe = 
  { recipe = Merge (Node defaultStep Empty)
                   (Node defaultStep2 (Node defaultStep Empty))
  , name = "Spaghetti arrabiata"
  , comments = "" }

toForm : (Float, Float) -> RecipeList -> Form
toForm (x, y) list =
  let blockSize = toFloat size
  in case list of
    Empty           -> 
      move (x,y) <| outlined (solid red) (square 10)
    Node step rest  ->
      group [ (move (x,y) <| Step.toForm step)
            , (toForm (x, y-blockSize) rest) ]
    Merge left right -> 
      group [ (toForm (     x, y-blockSize) left)
            , (toForm (x+blockSize, y-blockSize) right) ]

addRecipe : (Float, Float) -> Recipe -> Form -> Form
addRecipe (col, row) { recipe } board =
  let (dx, dy) = calculateMove(col, row)
      blockForm = toForm (0, 0) recipe |> move (dx, dy)
  in group [board, blockForm]

main = 
  let screen = addRecipe (0, 0) defaultRecipe Ingredient.initialBackground
  in toHtml <| 
    collage (round (cols*(toFloat size))) (round (rows*(toFloat size))) [screen]

