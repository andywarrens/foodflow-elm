module SearchBox exposing (..)

import Ingredient exposing (Ingredient, turkey, rijst, pezo)

import Html exposing (Html, Attribute, div, img, ul, li, input, text, node)
import Element exposing (show)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Json.Decode as Decode



main =
  Html.program
    { init = init "tomato"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { search      : String
  , ingredientsUrls : List String
  , ingredients : List Ingredient
  , selectedIngredient : Maybe Ingredient }

defaultModel : Model
defaultModel =
  { search          = ""
  , ingredientsUrls = []
  , ingredients     = [ turkey, rijst, pezo ]
  , selectedIngredient = Nothing }

init : String -> (Model, Cmd Msg)
init topic = (defaultModel, fetchImages topic)

toHtml : Ingredient -> Html Msg
toHtml ing = img [ src ing.img, alt ing.name ] []

ingredientUrlToHtml : String -> Maybe Ingredient -> Url -> Html Msg
ingredientUrlToHtml searchTopic selected url = 
    let className = case selected of
        Just ingr -> if (ingr.img == url) then "selected-image" else ""
        Nothing   -> ""
    in img [ src url
           , alt searchTopic
           , class className
           , onClick (SelectIngredient url)
           ] []

-- UPDATE

type alias Url = String
type Msg = TextInput String
         | NewImages (Result Http.Error (List String))
         | SelectIngredient Url

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
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

view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ stylesheet "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    , stylesheet "css/main.css"
    , input [ placeholder "Search for ingredients", onInput TextInput ] []
    , ul [] (List.map (ingredientUrlToHtml model.search model.selectedIngredient) model.ingredientsUrls)
    , ul [] (List.map toHtml model.ingredients)
    ]



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP
fetchImages : String -> Cmd Msg
fetchImages topic =
  let url = "http://127.0.0.1:8080/?image=" ++ topic
      request = Http.get url decodeUrls
  in Http.send NewImages request


decodeUrls : Decode.Decoder (List String)
decodeUrls =
  Decode.at ["data", "images"] (Decode.list Decode.string)



-- CSS
stylesheet url =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href" url 
            ]
        children = []
    in 
        node tag attrs children
