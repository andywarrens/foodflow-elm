module Recipe exposing (..)

import Step exposing (Step, defaultStep, toForm, width)
import Ingredient exposing (Ingredient
                            , turkey, oliveoil, pezo, artisjokhart, zongedroogdtomaten, littlegem, macadamia, rijst)

import Util exposing (tupleMap, size, calculateMove, Url)

import Html exposing (Html, text, div, ul, input, img)
import Html.Attributes exposing (placeholder, class, alt, src, value)
import Html.Events exposing (onInput, onClick)
import Element exposing (toHtml, show, flow, right, down)
import Collage exposing (Form, move, moveY, collage, toForm, group, solid, square, rect, circle, outlined)
import Color exposing (Color, red, blue)

import Http
import Json.Decode as Decode

import List exposing (singleton, map, append)

type RecipeList = End 
                | Node Step RecipeList  -- ofwel een node met een Step
                | Merge Recipe Recipe   -- ofwel een node die split

type alias Recipe = 
    { recipe   : RecipeList
    , name     : String
    , comments : String }

toForm : (Float, Float) -> Color -> Bool -> RecipeList -> Maybe Recipe -> Form
toForm (x, y) color isFirstRun list highlightRecipe =
  let blockSize = toFloat size
  in case list of
    End           -> 
      move (x,y) <| group [ moveY (-0.35*blockSize) <| outlined (solid color) (rect 1 (1.2*0.5*blockSize))
                          , outlined (solid red) (square 10) ]
    Node step rest  ->
      group [ move (x, y) <| outlined (solid color) (rect 1 (1.2*blockSize))
            , move (x+0.5*0.08*blockSize, y) <| outlined (solid color) (rect (0.08*blockSize) 1)
            , (move (x+blockSize*0.6,y) <| (Step.toForm >> Tuple.first) step)
            , (toForm (x, y+blockSize*1.2) color False rest highlightRecipe) ]
    Merge recipeLeft recipeRight -> 
      let (left, right) = (.recipe recipeLeft, .recipe recipeRight) 
          (maxWidth, nBranch) = recipeWidth left |> tupleMap toFloat
          stepMargin = maxWidth * blockSize
          branchMargin = (nBranch+1) * blockSize * 0.5
          margin = branchMargin + stepMargin
          (lineColorL, lineColorR) = case highlightRecipe of
              Nothing -> (blue, blue)
              Just a -> if (a == recipeLeft) then (red, color)
                        else if (a == recipeRight) then (color, red)
                        else (color, color)
          mergeBlock = if isFirstRun == False then (-0.05, 1.3) else (0.35, 0.5*1.2)
      in group <|
          [ mergeBlock |> \(a, b) -> move (x, y+a*blockSize) <| outlined (solid lineColorL) (rect 1 (b*blockSize))
          , move (x+0.5*margin, y+0.25*blockSize) <| outlined (solid color) <| rect margin 1
          , move (x+margin, y+(0.35*1.2*blockSize)) <| outlined (solid lineColorR) (rect 1 (0.3*1.2*blockSize))
          , toForm (x       , y+blockSize*1.2) lineColorL False left highlightRecipe
          , toForm (x+margin, y+blockSize*1.2) lineColorR False right highlightRecipe ]

-- recipeWidth: Returns the number of branches and total blocks width
recipeWidth : RecipeList -> (Int, Int)
recipeWidth list =
    case list of
        End -> (0, 0)
        Node step rest -> 
            let w = Step.width step
                (restW, branch) = recipeWidth rest
            in (max w restW, branch)
        Merge left right -> 
            let (restL, branchL) = recipeWidth (.recipe left)
                (restR, branchR) = recipeWidth (.recipe right)
            in (restL + restR, 1+branchL+branchR)

drawRecipe : (Float, Float) -> Recipe -> Maybe Recipe -> Form -> Form
drawRecipe (col, row) { recipe } selectedSubRecipe board =
  let (dx, dy) = calculateMove(col, row)
      recipeStart = outlined (solid blue) (circle 10) |> move (dx, dy)
      blockForm = toForm (0, 0) blue True recipe selectedSubRecipe |> move (dx, dy)
  in group [board, recipeStart, blockForm]

getSubRecipes : RecipeList -> List (Maybe RecipeList)
getSubRecipes recipe = case recipe of
        End -> singleton Nothing
        Node _ rest -> getSubRecipes rest
        Merge left right -> Just (Merge left right) :: append (getSubRecipes (.recipe left)) (getSubRecipes (.recipe right))

getSteps : RecipeList -> List Step
getSteps recipe =
    case recipe of
        End              -> []
        Node step rest   -> step :: getSteps rest
        Merge left right -> append (getSteps <| .recipe left)
                                   (getSteps <| .recipe right)

--- Example recipes
saladeKip : Recipe
saladeKip = 
  let substep2a = { ingredients = [ turkey, oliveoil, pezo ], action = "Gril 10min" }
      substep1a = { ingredients = [ artisjokhart ], action = "Gril extra 3min" }
      substep2b = { ingredients = [ zongedroogdtomaten, littlegem ], action = "Snijd in stukjes en reepjes" }
      substep1b = { ingredients = [ macadamia ], action = "Hak in grove stukjes" }
      riceStep =  { ingredients = [ rijst ], action = "Rijst koken" }
      step1a = Node substep1a (Node substep2a End)
      step1b = Node substep1b (Node substep2b End)
      subrecipe1a = { recipe= step1a, name= "Groenten grillen", comments = "" }
      subrecipe2 = { recipe= step1b, name= "Groenten snijden", comments = "" }
      subrecipe3 = { recipe= (Node riceStep End), name= "Rijst koken", comments = "" }
      subrecipe2a = { recipe = (Merge subrecipe2 subrecipe3), name = "Combineer groenten en rijst", comments = "" }
  in { recipe   = (Merge subrecipe1a subrecipe2a)
     , name     = "Salade met gegrilde kip"
     , comments = "" }

defaultStep2 : Step
defaultStep2 = 
    { ingredients =
        [ { name = "Spaghetti", qty = 500, unit = "gram", img = "img/spaghetti.jpg" }
        , { name = "Water", qty = 2, unit = "litre", img = "img/water.jpg" } ]
    , action = "Boil" }

defaultRecipe : Recipe
defaultRecipe = 
    let subrecipe1 = { recipe = (Node defaultStep (Node defaultStep End)),
                        name = "Meng groenten", comments = "" }
        subrecipe2 = { recipe = (Node defaultStep2 End)
                     , name = "Kook spaghetti", comments = "" }
        dummyrecipe1 = { recipe = Merge subrecipe1 subrecipe2
                       , name = "Meng groenten & kook spaghetti", comments = "" }
    in { recipe = Merge dummyrecipe1 subrecipe1
       , name = "Spaghetti arrabiata"
       , comments = "" }
