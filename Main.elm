module Main exposing (..)

import Recipe exposing (Recipe, drawRecipe, saladeKip)
import Ingredient exposing (Ingredient)

import Util exposing (tupleMap, stylesheet, stylesheetcdn, initialBackground,
                      size, cols, rows, Url)

import Html exposing (Html, text, div, h2, ul, li, a, input, img, i, hr)
import Html.Attributes exposing (placeholder, class, alt, src, value)
import Html.Events exposing (onInput, onClick, onMouseOver)
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
  , selectedRecipe     : Recipe 
  , selectedSubRecipe  : Maybe Recipe }

defaultModel : String -> Model
defaultModel searchTopic =
  { search             = searchTopic 
  , ingredientsUrls    = []
  , selectedIngredient = Nothing
  , selectedRecipe     = saladeKip 
  , selectedSubRecipe  = Nothing }

-- UPDATE
----------------------------------------
type Msg = RecipeMsg String -- dummy for test
         | SearchboxEvent SearchboxMsg
         | HoverSubRecipe Recipe
         | SelectRecipe Recipe
type SearchboxMsg = TextInput String
         | NewImages (Result Http.Error (List String))
         | SelectIngredient Url

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RecipeMsg str -> 
        (model, Cmd.none)
    HoverSubRecipe recipe ->
        ({ model | selectedSubRecipe = Just recipe }, Cmd.none)
    SelectRecipe recipe ->
        ({ model | selectedRecipe = recipe 
                 , selectedSubRecipe = Nothing }, Cmd.none)
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
  let screen = drawRecipe (0, 5) model.selectedRecipe model.selectedSubRecipe initialBackground
      width = round <| cols*(toFloat size)
      height = round <| rows*(toFloat size)
      screenHtml = toHtml <| collage width height [screen] 
  in div [ class "container" ]
    [ stylesheetcdn "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
        "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" 
        "anonymous"
    , stylesheet "css/main.css"
    , div [ class "row" ]
      [ div [ class "col-md-8" ] [ h2 [] [ text "Recipe: ", i [] [ text model.selectedRecipe.name ] ], screenHtml ]
      , div [ class "col-md-3 offset-md-1" ] 
        [ h2 [] [ text "Search:" ]
        , input [ placeholder "Search for ingredients"
                , onInput (SearchboxEvent << TextInput)
                , value model.search 
                , class "form-control" ] []
        , ul [] (List.map (li [] << List.singleton << ingredientUrlToHtml model.selectedIngredient) model.ingredientsUrls)
        ] ]
    , div [ class "row" ] 
      [ div [ class "col-md-3" ] 
        [ h2 [] [ text "Subrecipes:" ] 
        , buildSubRecipes model.selectedRecipe ]
      , div [ class "col-md-3" ] 
        [ h2 [] [ text "Steps:" ] 
        , buildStepsView model.selectedRecipe ]
      , div [ class "col-md-3" ] 
        [ h2 [] [ text "Selected:" ] 
        , div [] [ case model.selectedSubRecipe of
                    Nothing -> text ""
                    Just a  -> text << .name <| a ] ]
      ]
    , hr [] []
    , div [ class "row" ] 
      [ div [ class "col-md-3" ] 
        [ h2 [] [ text "Recipes:" ] 
        , ul [] 
            (List.map (\recipe -> li [] [ a [onClick <| SelectRecipe recipe] [text << .name <| recipe] ])
              [ saladeKip, Recipe.defaultRecipe ])
        ]
      ]
    ]

buildSubRecipes : Recipe -> Html Msg
buildSubRecipes { recipe } = 
    let subrecipes = Recipe.getSubRecipes recipe
        createLi recipe = li [onMouseOver (HoverSubRecipe recipe)] [text <| .name recipe]
        subRecipesVisual = 
            List.filter (\a -> case a of
                Nothing -> False 
                Just _  -> True) subrecipes
            |> List.map (\a -> case a of
                Nothing -> Recipe.End 
                Just b  -> b)
            |> List.concatMap (\a -> case a of 
                Recipe.End               -> [text "end"]
                Recipe.Node _ _          -> [text "step"]
                Recipe.Merge left right      -> [createLi left, createLi right])
    in ul [] subRecipesVisual

--extractName recipeList = case recipeList of 
--    Recipe.End               -> ["end"]
--    Recipe.Node _ _          -> ["step"]
--    Recipe.Merge left right      -> [.name left, .name right]
--    Recipe.BeginMerge left right -> [.name left, .name right]

buildStepsView : Recipe -> Html Msg
buildStepsView { recipe } = 
    let steps = Recipe.getSteps recipe
                |> List.map .action
        buildLi = text >> singleton >> li []
    in ul [] (List.map buildLi steps)

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
