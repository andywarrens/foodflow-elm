module Recipe exposing (..)

import Step exposing (Step, defaultStep, toForm, width)
import Ingredient exposing (cols, rows, size, calculateMove)

import Html exposing (text, div, p)
import Html.Attributes exposing (class)
import Element exposing (toHtml, show, flow, right, down)
import Collage exposing (Form, move, moveY, collage, toForm, group, solid, square, rect, circle, outlined)
import Color exposing (red, blue)

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
      move (x,y) <| group [ moveY (-0.35*blockSize) <| outlined (solid blue) (rect 1 (1.2*0.5*blockSize))
                          , outlined (solid red) (square 10) ]
    Node step rest  ->
      group [ move (x, y) <| outlined (solid blue) (rect 1 (1.2*blockSize))
            , (move (x+blockSize*0.6,y) <| Step.toForm step)
            , (toForm (x, y+blockSize*1.2) rest) ]
    Merge left right -> 
      let maxWidth = (recipeWidth left) + 1
      in group 
        [ move (x, y+0.35*blockSize) <| outlined (solid blue) (rect 1 (0.5*1.2*blockSize))
        , move (x+0.5*(toFloat maxWidth)*blockSize, y+0.25*blockSize) <| outlined (solid blue) <| rect ((toFloat maxWidth)*blockSize) 1
        , move (x+(toFloat maxWidth)*blockSize, y+(0.325*1.2*blockSize)) <| outlined (solid blue) (rect 1 (0.25*1.2*blockSize))
        , (toForm (     x, y+blockSize) left)
        , (toForm (x+(toFloat maxWidth)*blockSize, y+blockSize) right) ]

recipeWidth : RecipeList -> Int
recipeWidth list =
    case list of
        Empty -> 0
        Node step rest -> (Step.width step) + recipeWidth rest
        Merge left right -> (recipeWidth left) + (recipeWidth right)

addRecipe : (Float, Float) -> Recipe -> Form -> Form
addRecipe (col, row) { recipe } board =
  let (dx, dy) = calculateMove(col, row)
      recipeStart = outlined (solid blue) (circle 10) |> move (dx, dy)
      blockForm = toForm (0, 0) recipe |> move (dx, dy)
  in group [board, recipeStart, blockForm]

main = 
  let screen = addRecipe (0, 5) defaultRecipe Ingredient.initialBackground
  in toHtml <| 
    collage (round (cols*(toFloat size))) (round (rows*(toFloat size))) [screen]

