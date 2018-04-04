module Recipe exposing (..)

import Step exposing (Step, defaultStep, toForm, width)
import Ingredient exposing (Ingredient, cols, rows, size, calculateMove
                            , turkey, oliveoil, pezo, artisjokhart, zongedroogdtomaten, littlegem, macadamia, rijst)

import Util exposing (tupleMap, stylesheet, stylesheetcdn)

import Html exposing (Html, text, div, ul, input, img)
import Html.Attributes exposing (placeholder, class, alt, src, value)
import Html.Events exposing (onInput, onClick)
import Element exposing (toHtml, show, flow, right, down)
import Collage exposing (Form, move, moveY, collage, toForm, group, solid, square, rect, circle, outlined)
import Color exposing (red, blue)

import Http
import Json.Decode as Decode

import List exposing (singleton, map)

main =
  Html.program
    { init = (defaultModel "pear", fetchImages "pear")
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

type RecipeList = Empty 
                | Node Step RecipeList          -- ofwel een node met een Step
                | Merge RecipeList RecipeList   -- ofwel een node die split
                | BeginMerge RecipeList RecipeList -- puur voor iets andere layout

type alias Recipe = 
    { recipe   : RecipeList
    , name     : String
    , comments : String }

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

-- MODEL

type alias Url = String
type alias Model =
  { search             : String
  , ingredientsUrls    : List String
  , selectedIngredient : Maybe Ingredient
  , selectedRecipe     : Recipe }

defaultModel : String -> Model
defaultModel searchTopic =
  { search             = searchTopic 
  , ingredientsUrls    = []
  , selectedIngredient = Nothing
  , selectedRecipe     = saladeKip }

type Msg = RecipeMsg String -- dummy for test
         | SearchboxEvent SearchboxMsg
type SearchboxMsg = TextInput String
         | NewImages (Result Http.Error (List String))
         | SelectIngredient Url

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RecipeMsg str -> (model, Cmd.none)
    SearchboxEvent evt -> case evt of
        TextInput newContent ->
          ({ model | search = newContent }, fetchImages newContent)

        NewImages (Ok newUrls) ->
          ({ model | ingredientsUrls = newUrls }, Cmd.none)

        NewImages (Err _) ->
          ({ model | ingredientsUrls = ["error"] }, Cmd.none)

        SelectIngredient url ->
            let ingr = Ingredient model.search 1 "piece" url
            in ({ model | selectedIngredient = Just ingr }, Cmd.none)

view : Model -> Html Msg
view model =
  let screen = addRecipe (0, 5) model.selectedRecipe Ingredient.initialBackground
      width = round <| cols*(toFloat size)
      height = round <| rows*(toFloat size)
      screenHtml = toHtml <| collage width height [screen] 
  in div [ class "container" ]
    [ stylesheetcdn "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
        "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" 
        "anonymous"
    , stylesheet "css/main.css"
    , div [ class "row" ]
      [ div [ class "col-md-8" ] [ screenHtml ]
      , div [ class "col-md-4" ] 
        [ input [ placeholder "Search for ingredients", onInput (SearchboxEvent << TextInput), value model.search ] []
        , ul [] (List.map (ingredientUrlToHtml model.search model.selectedIngredient) model.ingredientsUrls)
        ] ] ]

ingredientUrlToHtml : String -> Maybe Ingredient -> Url -> Html Msg
ingredientUrlToHtml searchTopic selected url = 
    let className = case selected of
        Just ingr -> if (ingr.img == url) then "selected-image" else ""
        Nothing   -> ""
    in img [ src url
           , alt searchTopic
           , class className
           , onClick (SearchboxEvent <| SelectIngredient url)
           ] []
-- HTTP
fetchImages : String -> Cmd Msg
fetchImages topic =
  let url = "http://127.0.0.1:8080/?image=" ++ topic
      request = Http.get url decodeUrls
  in Http.send (SearchboxEvent << NewImages) request


decodeUrls : Decode.Decoder (List String)
decodeUrls =
  Decode.at ["data", "images"] (Decode.list Decode.string)

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

defaultStep2 : Step
defaultStep2 = 
    { ingredients =
        [ { name = "Spaghetti", qty = 500, unit = "gram", img = "img/spaghetti.jpg" }
        , { name = "Water", qty = 2, unit = "litre", img = "img/water.jpg" } ]
    , action = "Boil" }


defaultRecipe : Recipe
defaultRecipe = 
  { recipe = BeginMerge (Merge (Node defaultStep (Node defaultStep Empty))
                          (Node defaultStep2 Empty))
                   (Node defaultStep2 (Node defaultStep (Merge Empty Empty)))
  , name = "Spaghetti arrabiata"
  , comments = "" }
