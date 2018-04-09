module Main exposing (..)

import Recipe exposing (Recipe, addRecipe, saladeKip)
import Ingredient exposing (Ingredient)

import Util exposing (tupleMap, stylesheet, stylesheetcdn, initialBackground,
                      size, cols, rows, Url)

import Html exposing (Html, text, div, ul, li, input, img)
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

-- MODEL
----------------------------------------
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

-- UPDATE
----------------------------------------
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

-- VIEW
----------------------------------------
view : Model -> Html Msg
view model =
  let screen = addRecipe (0, 5) model.selectedRecipe initialBackground
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
      , div [ class "col-md-3 offset-md-1" ] 
        [ input [ placeholder "Search for ingredients"
                , onInput (SearchboxEvent << TextInput)
                , value model.search 
                , class "form-control" ] []
        , ul [] (List.map (li [] << List.singleton << ingredientUrlToHtml model.selectedIngredient) model.ingredientsUrls)
        ] ] ]

ingredientUrlToHtml : Maybe Ingredient -> Url -> Html Msg
ingredientUrlToHtml selected url = 
    let className = case selected of
            Just ingr -> if (ingr.img == url) then "selected-image" else ""
            Nothing   -> ""
        displayAlt = case selected of
            Just ingr -> ingr.name
            Nothing   -> ""
    in img [ src url
           , alt displayAlt
           , class className
           , onClick (SearchboxEvent <| SelectIngredient url)
           ] []

-- HTTP
----------------------------------------
fetchImages : String -> Cmd Msg
fetchImages topic =
  let url = "http://127.0.0.1:8080/?image=" ++ topic
      request = Http.get url decodeUrls
  in Http.send (SearchboxEvent << NewImages) request


decodeUrls : Decode.Decoder (List String)
decodeUrls =
  Decode.at ["data", "images"] (Decode.list Decode.string)

-- SUBSCRIPTIONS
----------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
