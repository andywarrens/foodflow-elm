module Main exposing (..)

import Recipe exposing (Recipe, saladeKip)
import Step exposing (Step, toForm)
import Ingredient exposing (Ingredient)

import Util exposing (Url, stylesheet, stylesheetcdn, initialBackground, size, cols, rows)

import Html exposing (Html, text, div, h2, ul, li, a, input, img, hr)
import Html.Attributes exposing (placeholder, class, alt, src, value)
import Html.Events exposing (onInput, onClick, onMouseEnter, onMouseLeave)
import Element exposing (toHtml, show, flow, right, down)
import Collage exposing (collage)

import Http
import Json.Decode as Decode

import List exposing (singleton, map)
import Either exposing (Either)


main =
  Html.program
    { init = "orange juice" |> \a -> (defaultModel a, fetchImages a)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
----------------------------------------
type alias Model =
  { search             : String
  , ingredientsUrls    : Either (List Url) String
  , hoverIngredient    : Maybe Ingredient
  , selectedStep       : Maybe Step
  , selectedRecipe     : Recipe  }

defaultModel : String -> Model
defaultModel searchTopic =
  { search             = searchTopic 
  , ingredientsUrls    = Either.Left []
  , hoverIngredient    = Nothing
  , selectedStep       = Nothing
  , selectedRecipe     = saladeKip }


-- UPDATE
----------------------------------------
type Msg = SearchboxEvent SearchboxMsg
         | SelectStep Step
         | SelectRecipe Recipe
type SearchboxMsg = TextInput String
         | NewImages (Result Http.Error (List String))
         | HoverIngredient (Maybe Url)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectStep step -> 
        (model, Cmd.none)
    SelectRecipe recipe ->
        ({ model | selectedRecipe = recipe}, Cmd.none)
    SearchboxEvent evt -> case evt of
        TextInput newContent ->
          ({ model | search = newContent }, fetchImages newContent)

        NewImages (Ok newUrls) ->
          ({ model | ingredientsUrls = Either.Left newUrls }, Cmd.none)

        NewImages (Err _) ->
          ({ model | ingredientsUrls = Either.Right "error while fetching" }, Cmd.none)

        HoverIngredient url ->
            case url of
                Just a -> 
                  let ingr = Ingredient model.search 1 "piece" a
                  in ({ model | hoverIngredient = Just ingr }, Cmd.none)
                Nothing -> ({ model | hoverIngredient = Nothing }, Cmd.none)

-- VIEW
----------------------------------------
view : Model -> Html Msg
view model =
  let steps = Recipe.getSteps model.selectedRecipe.recipe
      (board, _) = List.foldr 
        (\step (board, i) -> (Step.addStep (0, i) step board, i+1))
        (initialBackground, 0) steps
      width = round <| cols*(toFloat size)
      height = round <| rows*(toFloat size)
      screenHtml = toHtml <| collage width height [board]
  in div [ class "container" ]
    [ stylesheetcdn "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
        "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" 
        "anonymous"
    , stylesheet "css/main.css"
    , div [ class "row" ]
      [ div [ class "col-md-8" ] 
        [ h2 [] [ text "Recipe builder: ", input [ value model.selectedRecipe.name ] [] ]
        , screenHtml ]
      , div [ class "col-md-3 offset-md-1" ] 
        [ h2 [] [ text "Search:" ]
        , input [ placeholder "Search for ingredients"
                , onInput (SearchboxEvent << TextInput)
                , value model.search 
                , class "form-control" ] []
        , ul [] (ingredientUrlsToHtml model.hoverIngredient model.ingredientsUrls) ]
      ]
    , div [ class "row" ] 
      [ div [ class "col-md-3" ] 
        [ h2 [] [ text "Steps:" ] 
        , buildStepsView model.selectedRecipe ]
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

buildStepsView : Recipe -> Html Msg
buildStepsView { recipe } = 
    let steps = Recipe.getSteps recipe
                |> List.map (.action >> text >> singleton >> li [])
    in ul [] steps

ingredientUrlsToHtml : Maybe Ingredient -> Either (List Url) String -> List (Html Msg)
ingredientUrlsToHtml sel list =
    case list of
        Either.Right error -> [li [] [ text ("error fetching urls: " ++ error) ]]
        Either.Left urls -> List.map (li [] << List.singleton << ingredientUrlToHtml sel) urls

ingredientUrlToHtml : Maybe Ingredient -> Url -> Html Msg
ingredientUrlToHtml sel url = 
  let (className, displayAlt) = case sel of
      Nothing -> ("", "")
      Just a -> ( if (a.img == url) then "selected-image" else ""
                , if (a.img == url) then a.name else "" )
  in img [ src url
         , alt displayAlt
         , class className
         , onMouseEnter (SearchboxEvent <| HoverIngredient (Just url))
         , onMouseLeave (SearchboxEvent <| HoverIngredient (Nothing))
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

