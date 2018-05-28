module Parser exposing (..)

import Recipe exposing (Recipe, drawRecipe, defaultRecipe, RecipeList(..))
import Step exposing (Step)
import Ingredient exposing (..)

import Util exposing (tupleMap, stylesheet, stylesheetcdn, initialBackground,
                      size, cols, rows, Url)

import Html exposing (Html, text, div, h2, ul, li, a, input, img, i, hr, button, textarea)
import Html.Attributes exposing (placeholder, class, alt, src, value, type_, style)
import Html.Events exposing (onInput, onClick, onMouseOver)
import Element exposing (toHtml, show, flow, right, down)
import Collage exposing (Form, move, moveY, collage, toForm, group, solid, square, rect, circle, outlined)
import Color exposing (red, blue)

import String exposing (fromList, toList, left, dropLeft, trim, length)
import List exposing (singleton, map)
import List.Extra exposing (takeWhile, dropWhile)
import Dict exposing (Dict)
import Regex exposing (Regex, regex, caseInsensitive, contains, find, HowMany(..))

main = Html.program
  { init = initialModel ! []
  , update = update
  , subscriptions = \model -> Sub.none
  , view = view }

type Msg = NoOp | TextInput String

update msg model =
    case msg of
        NoOp -> ("Start", Cmd.none)
        TextInput text -> (text, Cmd.none)

view model = 
  let parsedRecipe = parse model
      screenHtml = case parsedRecipe of
          Ok recipe -> let screen = drawRecipe (0, 5) recipe Nothing initialBackground
                           width = round <| cols*(toFloat size)
                           height = round <| rows*(toFloat size)
                        in toHtml <| collage width height [screen] 
          Err msg -> text msg
  in div [ class "container" ]
    [ stylesheetcdn "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
        "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" 
        "anonymous"
    , stylesheet "css/main.css"
    , div [ class "row" ]
      [ h2 [] [ text model ]
      ]
    , div [ class "row" ]
      [ div [ class "col-md-8" ] [ screenHtml ]
      , div [ class "col-md-3 offset-md-1" ] 
        [ ]
      , hr [] []
      ]
    , div [ class "row" ]
      [ div [ class "col-md-8" ] 
        [ textarea [ placeholder "Enter a recipe"
                , onInput TextInput
                , class "form-control"
                , value model
                , Html.Attributes.rows 6 ] []
        ]
      ]
    ]


initialModel : String
initialModel = 
  [ "a = Step [rice]"
  , "b = Step [tomato, littlegem]"
  , "  -> Step [macadamia]"
  , "c = Step [turkey]"
  , "Recipe = a + b"]
  --|> List.foldl (\a v -> a ++ v ++ "\n") ""
  |> String.join "\n"

type ParserState = 
      WfVariable
    | DoneWfVariable String
    | WfValue
    | WfIngredients Int
    | CollectingIngredients String
    | DoneCollectingIngredients String
    | WfNextStep
    | FoundArrow Int
    | SkipUntilAfter Char ParserState
    | WfRecipe 
    | OperatorFound Operator
    | Done (Result String Recipe)
type Operator = MergeOp
type Flag =  None | Arrow | Recipe | Merging RecipeList

type alias ParsingState = 
    { waiting     : ParserState
    , currentVar  : String
    , varStack    : String
    , flag        : Flag
    , recipelists : Dict String RecipeList }

parse : String -> Result String Recipe
parse recipe = parser (String.lines recipe |> String.join "") defaultParsingState

defaultParsingState : ParsingState
defaultParsingState = { waiting = SkipUntilAfter ' ' WfVariable
                      , currentVar = ""
                      , varStack = ""
                      , flag = None
                      , recipelists = Dict.empty }

-- 2-phase state machine
--   1) Determine next state
--   2) Change outputs (Vars, Dict, Flags) based on prev/next state
--
-- Is not complete: at the end it just makes a merge of all the vars
--
parser : String -> ParsingState -> Result String Recipe
parser recipe state =
    let first = String.left 1 recipe
        rest = String.dropLeft 1 recipe
        prevState = state.waiting
        nextState = case prevState of
          WfVariable ->
            if (allowedI first (regex "[a-z0-9]") == True) then WfVariable 
            else case first of
                "+" -> OperatorFound MergeOp
                " " -> SkipUntilAfter ' ' WfVariable
                "=" -> WfValue
                _   -> Done <| Err ("WfVariable not allowed: " ++ first)
          WfValue -> 
            let stepIx = (indexOf "Step \\[" recipe) + (String.length "Step [")
                step = left stepIx recipe
                varIx = (indexOf "[a-z0-9]" recipe)
            in if (trim step == "Step [") then WfIngredients stepIx
                else Done <| Err ("WfValue: Expected 'Step [' but got: " ++ step)
          CollectingIngredients ingrs ->
            if (allowedI first (regex "[a-z, ]") == True) then CollectingIngredients (ingrs ++ first)
            else if (first == "]") then DoneCollectingIngredients ingrs
            else Done <| Err ("CollectingIngredients not allowed: " ++ first)
          WfNextStep ->
            let arrowIx = indexOf "->" recipe
                arrow = case arrowIx of
                    -1 -> ""
                    _  -> left (arrowIx+2) recipe
                _ = Debug.log "wfNextStep arrow?" arrow
            in if (trim arrow == "->") then FoundArrow (arrowIx+2)
               else SkipUntilAfter ' ' WfVariable
          WfRecipe -> WfRecipe
          SkipUntilAfter v r -> SkipUntilAfter v r
          _ -> Done <| Err "unknown state"
     in case nextState of
         Done result -> result
         WfVariable ->
            let flag = case prevState of WfRecipe -> Recipe
                                         _        -> state.flag
            in parser rest { state | waiting = nextState
                                   , varStack = state.varStack ++ first 
                                   , flag = flag }
         WfValue ->
             let _ = Debug.log "wfValue" state.varStack
                 nextS =  case state.varStack of
                            "Recipe" -> SkipUntilAfter ' ' WfRecipe
                            _ -> SkipUntilAfter ' ' WfValue 
             in parser rest { state | varStack = ""
                                    , flag = None
                                    , currentVar = state.varStack
                                    , waiting = nextS }
         WfIngredients pos -> 
           let remaining = dropLeft pos recipe
           in parser remaining { state | waiting = (CollectingIngredients "") }
         DoneCollectingIngredients ingrs ->
           let list = String.split "," ingrs |> List.map (trim >> toIngredient)
               step = Step list ""
               newRecipeLists = 
                   case state.flag of
                     Arrow -> 
                         let lastStep = Dict.get state.currentVar state.recipelists
                             _ = Debug.log "DoneCollectingIngr after arrow" state.currentVar
                         in case lastStep of
                             Nothing -> Dict.empty
                             Just s  -> 
                               let newStep = Recipe.addStep s step
                               in Dict.insert state.currentVar newStep state.recipelists
                     _ -> Dict.insert state.currentVar (Node step End) state.recipelists
               _ = Debug.log "DoneColl" (toString newRecipeLists)
            in if newRecipeLists == Dict.empty
               then Err <| "Unknown variable lookup: " ++ state.currentVar
               else parser rest { state | recipelists = newRecipeLists, waiting = WfNextStep }
         FoundArrow pos ->
             let remaining = dropLeft pos recipe
                 _ = Debug.log "Foundarrow" ""
             in parser remaining { state | waiting = WfValue
                                         , flag = Arrow }
         SkipUntilAfter value resumeState ->
             let remaining = dropWhile ((==) value) (toList recipe) |> fromList
                 diff = (length recipe) - (length remaining)
                 --_ = Debug.log ("skipUntilAfter (" ++ toString value ++ ") dropped") (toString diff)
                 --_ = Debug.log "remaining" remaining
             in parser remaining { state | waiting = resumeState }
         OperatorFound op ->
             let foundVar = state.currentVar ++ first
                 var = Dict.get foundVar state.recipelists
             in case var of
               Nothing -> Err <| "Unknown variable lookup: " ++ foundVar
               Just s  -> parser rest { state | flag = Merging s
                                              , waiting = SkipUntilAfter ' ' prevState }
         WfRecipe ->
            let recipe = Dict.foldl mergeWithRecipe (Recipe.Recipe End "Startrecipe" "") state.recipelists
            in Ok recipe
         _ -> parser rest { state | waiting = nextState }

mergeWithRecipe : String -> RecipeList -> Recipe -> Recipe
mergeWithRecipe name rl acc =
  let recipe = Recipe.Recipe rl name ""
      otherRecipeName = case acc.recipe of
          End -> "end"
          Node _ _ -> "node"
          Merge l r -> (l.name ++ " ") ++ r.name
  in Recipe.Recipe (Merge recipe acc) otherRecipeName ""

allowedI : String -> Regex -> Bool
allowedI val reg = contains (caseInsensitive reg) val

toIngredient : String -> Ingredient
toIngredient name = Ingredient name 0 "piece" ("img/" ++ name ++ ".jpg")

indexOf : String -> String -> Int
indexOf reg value = case find (AtMost 1) (regex reg) value of
    match::_ -> match.index
    []       -> -1
