module Recipe exposing (..)

import Step exposing (Step, defaultStep, toForm, width,
    cutOnion, cutTomato, cutChicken, heatPan)
import Ingredient exposing (Ingredient
                            , turkey, oliveoil, pezo, artisjokhart, zongedroogdtomaten, littlegem, macadamia, rijst)

import Util exposing (tupleMap, size, calculateMove, Url, solid)

import Html exposing (Html, text, div, ul, input, img)
import Html.Attributes exposing (placeholder, class, alt, src, value)
import Html.Events exposing (onInput, onClick)

import Collage exposing (Collage, image, group, outlined, shift, shiftY, uniform)
import Collage exposing (square, rectangle, circle)
import Collage exposing (filled)
import Color exposing (Color, red, blue)

import Http
import Json.Decode as Decode

import List exposing (singleton, map, append)
import Step exposing (defaultStep2)

type RecipeList = End 
                | Node Step RecipeList  -- ofwel een node met een Step
                | Merge Recipe Recipe   -- ofwel een node die split

type alias Recipe = 
    { recipe   : RecipeList
    , name     : String
    , comments : String }

toForm : (Float, Float) -> Color -> Bool -> RecipeList -> Maybe Recipe -> Collage msg
toForm (x, y) color isFirstRun list highlightRecipe =
  let blockSize = toFloat size
  in case list of
    End           -> 
      shift (x,y) <| group [ outlined (solid red) (square 10) 
                           , shiftY (-0.35*blockSize) <| outlined (solid color) (rectangle 1 (1.2*0.5*blockSize))
                           ]
    Node step rest  ->
        let bulletWidth = 0.08*blockSize
            moveStepToMiddle (s, width) = shift (x+0.5*width+bulletWidth+10,y) s
        in group [ shift (x, y) <| outlined (solid color) (rectangle 1 (1.2*blockSize))
            , shift (x+0.5*bulletWidth, y) <| outlined (solid color) (rectangle bulletWidth 1)
            , step |> Step.toForm >> moveStepToMiddle
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
          [ mergeBlock |> \(a, b) -> shift (x, y+a*blockSize) <| outlined (solid lineColorL) (rectangle 1 (b*blockSize))
          , shift (x+0.5*margin, y+0.25*blockSize) <| outlined (solid color) <| rectangle margin 1
          , shift (x+margin, y+(0.35*1.2*blockSize)) <| outlined (solid lineColorR) (rectangle 1 (0.3*1.2*blockSize))
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

drawRecipe : (Float, Float) -> Recipe -> Maybe Recipe -> Collage msg -> Collage msg
drawRecipe (col, row) { recipe } selectedSubRecipe board =
  let (dx, dy) = calculateMove(col, row)
      recipeStart = outlined (solid blue) (circle 10) |> shift (dx, dy)
      blockForm = toForm (0, 0) blue True recipe selectedSubRecipe |> shift (dx, dy)
  in group [blockForm, recipeStart, board]

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

addStep : RecipeList -> Step -> RecipeList
addStep recipe new =
    case recipe of
        End              -> Node new End
        Node step rest   -> Node new (Node step rest)
        Merge left right -> Node new (Merge left right)

insertStep : Int -> Step -> RecipeList -> RecipeList
insertStep pos new recipe =
    if pos == 0 then
       addStep recipe new
    else
       case recipe of 
         End              -> Node new End
         Node step rest   -> Node step (insertStep (pos-1) new rest)
         Merge left right -> 
           let newLeft = { left | recipe = insertStep (pos-1) new left.recipe }
           in Merge newLeft right

removeAtStep : Int -> RecipeList -> (Maybe Step, RecipeList)
removeAtStep pos recipe =
  case recipe of
    End              -> (Nothing, End)
    Node step rest   -> 
      if pos==0 then (Just step, rest)
      else let (resultStep, resultTree) = removeAtStep (pos-1) rest
           in (resultStep, Node step resultTree)
    Merge left right -> 
      let (resultStep, resultTree) = removeAtStep (pos-1) left.recipe
          newLeft = { left | recipe = resultTree }
      in (resultStep, Merge newLeft right)

moveStep : RecipeList -> Int -> Int -> RecipeList
moveStep recipe from to =
  let (step, tree) = removeAtStep from recipe
  in case step of
      Just x  -> insertStep to x tree
      Nothing -> tree

changeStep : RecipeList -> Step -> Step -> RecipeList
changeStep r select new = case r of
    End -> End
    Node step rest -> if (step == select) 
        then Node new rest
        else Node step (changeStep rest select new)
    Merge left right -> 
        let rLeft  = changeStep left.recipe select new
            rRight = changeStep right.recipe select new
        in Merge { left | recipe = rLeft } { right | recipe = rRight }

--- Example recipes
substep1a : Step
substep1a = { ingredients = [ turkey, oliveoil, pezo ], action = "0. Gril 10min" }

easySalad : Recipe
easySalad =
  let step2 = { ingredients = [ zongedroogdtomaten, littlegem ], action = "1. Snijd in stukjes en reepjes" }
      step3 = { ingredients = [ macadamia ], action = "2. Hak in grove stukjes" }
      step4 =  { ingredients = [ rijst ], action = "3. Rijst koken" }
  in { recipe   = Node substep1a (Node step2 (Node step3 (Node step4 End)))
     , name     = "Eenvoudige salade met gegrilde kip"
     , comments = "" }

saladeKip : Recipe
saladeKip = 
  let substep2a = { ingredients = [ turkey, oliveoil, pezo ], action = "Gril 10min" }
      substep2b = { ingredients = [ zongedroogdtomaten, littlegem ], action = "Snijd in stukjes en reepjes" }
      substep1b = { ingredients = [ macadamia ], action = "Hak in grove stukjes" }
      riceStep =  { ingredients = [ rijst ], action = "Rijst koken" }
      step1a = Node substep2a End
      step1b = Node substep1b (Node substep2b End)
      subrecipe1a = { recipe= step1a, name= "Groenten grillen", comments = "" }
      subrecipe2 = { recipe= step1b, name= "Groenten snijden", comments = "" }
      subrecipe3 = { recipe= (Node riceStep End), name= "Rijst koken", comments = "" }
      subrecipe2a = { recipe = (Merge subrecipe2 subrecipe3), name = "Combineer groenten en rijst", comments = "" }
  in { recipe   = (Merge subrecipe1a subrecipe2a)
     , name     = "Salade met gegrilde kip"
     , comments = "" }

defaultRecipe : Recipe
defaultRecipe = 
    let cutVeggies = { recipe = (Node cutOnion (Node cutTomato End) )
                     , name = "Snij groenten", comments = "" }
        cutChickenRec = { recipe = (Node cutChicken End)
                     , name = "Snij kip", comments = "" }
        mengAll = Merge cutVeggies cutChickenRec
        bakGroentenKip = { recipe = Node heatPan mengAll,
                           name = "Bak groenten en kip", comments = "" }
        kookSpaghetti = { recipe = (Node defaultStep2 End)
                     , name = "Kook spaghetti", comments = "" }
    in { recipe = Merge bakGroentenKip kookSpaghetti
       , name = "Spaghetti arrabiata"
       , comments = "" }

recipes : List Recipe
recipes = [ defaultRecipe, saladeKip, easySalad ]