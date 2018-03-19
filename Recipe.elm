module Recipe exposing (..)

import Step exposing (Step, defaultStep, toForm, width)
import Ingredient exposing (cols, rows, size, calculateMove
                            , turkey, oliveoil, pezo, artisjokhart, zongedroogdtomaten, littlegem, macadamia, rijst)
import Util exposing (tupleMap)

import Html exposing (Html, text, div, p)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Element exposing (toHtml, show, flow, right, down)
import Collage exposing (Form, move, moveY, collage, toForm, group, solid, square, rect, circle, outlined)
import Color exposing (red, blue)

import List exposing (singleton, map)

main =
  Html.beginnerProgram { model = model, view = view, update = update }


defaultStep2 : Step
defaultStep2 = 
    { ingredients =
        [ { name = "Spaghetti", qty = 500, unit = "gram", img = "img/spaghetti.jpg" }
        , { name = "Water", qty = 2, unit = "litre", img = "img/water.jpg" } ]
    , action = "Boil" }

type RecipeList = Empty 
                | Node Step RecipeList          -- ofwel een node met een Step
                | Merge RecipeList RecipeList   -- ofwel een node die split
                | BeginMerge RecipeList RecipeList -- puur voor iets andere layout

type alias Recipe = 
    { recipe   : RecipeList
    , name     : String
    , comments : String }

defaultRecipe : Recipe
defaultRecipe = 
  { recipe = BeginMerge (Merge (Node defaultStep (Node defaultStep Empty))
                          (Node defaultStep2 Empty))
                   (Node defaultStep2 (Node defaultStep (Merge Empty Empty)))
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
            , move (x+0.5*0.08*blockSize, y) <| outlined (solid blue) (rect (0.08*blockSize) 1)
            , (move (x+blockSize*0.6,y) <| Step.toForm step)
            , (toForm (x, y+blockSize*1.2) rest) ]
    Merge left right -> 
      let (maxWidth, nBranch) = recipeWidth left |> tupleMap toFloat
          stepMargin = maxWidth * blockSize
          branchMargin = (nBranch+1) * blockSize * 0.5
          margin = branchMargin + stepMargin
      in group 
        [ move (x, y-0.05*blockSize) <| outlined (solid blue) (rect 1 (1.3*blockSize))
        , move (x+0.5*margin, y+0.25*blockSize) <| outlined (solid blue) <| rect margin 1
        , move (x+margin, y+(0.35*1.2*blockSize)) <| outlined (solid blue) (rect 1 (0.3*1.2*blockSize))
        , (toForm (x       , y+blockSize*1.2) left)
        , (toForm (x+margin, y+blockSize*1.2) right) ]
    BeginMerge left right ->
      let (maxWidth, nBranch) = recipeWidth left |> tupleMap toFloat
          stepMargin = maxWidth * blockSize
          branchMargin = (nBranch+1) * blockSize * 0.5
          margin = branchMargin + stepMargin
      in group 
        [ move (x, y+0.35*blockSize) <| outlined (solid blue) (rect 1 (0.5*1.2*blockSize))
        , move (x+0.5*margin, y+0.25*blockSize) <| outlined (solid blue) <| rect margin 1
        , move (x+margin, y+(0.325*1.2*blockSize)) <| outlined (solid blue) (rect 1 (0.25*1.2*blockSize))
        , (toForm (x       , y+blockSize*1.2) left)
        , (toForm (x+margin, y+blockSize*1.2) right) ]

-- recipeWidth: Returns the number of branches and total blocks width
recipeWidth : RecipeList -> (Int, Int)
recipeWidth list =
    case list of
        Empty -> (0, 0)
        Node step rest -> 
            let w = Step.width step
                (restW, branch) = recipeWidth rest
            in (max w restW, branch)
        Merge left right -> 
            let (restL, branchL) = recipeWidth left
                (restR, branchR) = recipeWidth right
            in (restL + restR, 1+branchL+branchR)
        BeginMerge left right ->
            let (restL, branchL) = recipeWidth left
                (restR, branchR) = recipeWidth right
            in (restL + restR, 1+branchL+branchR)

addRecipe : (Float, Float) -> Recipe -> Form -> Form
addRecipe (col, row) { recipe } board =
  let (dx, dy) = calculateMove(col, row)
      recipeStart = outlined (solid blue) (circle 10) |> move (dx, dy)
      blockForm = toForm (0, 0) recipe |> move (dx, dy)
  in group [board, recipeStart, blockForm]

----

type alias Model = Recipe
model : Model
model = saladeKip

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model = model

view : Model -> Html Msg
view model =
  let screen = addRecipe (0, 5) model Ingredient.initialBackground
  in toHtml <| collage (round (cols*(toFloat size))) (round (rows*(toFloat size))) [screen]

--- Example recipes
saladeKip : Recipe
saladeKip = 
  let substep2a = { ingredients = [ turkey, oliveoil, pezo ], action = "Gril 10min" }
      substep1a = { ingredients = [ artisjokhart ], action = "Gril extra 3min" }
      substep2b = { ingredients = [ zongedroogdtomaten, littlegem ], action = "Snijd in stukjes en reepjes" }
      substep1b = { ingredients = [ macadamia ], action = "Hak in grove stukjes" }
      riceStep =  { ingredients = [ rijst ], action = "Rijst koken" }
      step1a = Node substep1a (Node substep2a Empty)
      step1b = Node substep1b (Node substep2b Empty)
  in { recipe   = (BeginMerge step1a (Merge step1b (Node riceStep Empty)))
     , name     = "Salade met gegrilde kip"
     , comments = "" }
