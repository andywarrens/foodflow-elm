module SearchBox exposing (..)

import Ingredient exposing (Ingredient, turkey, rijst, pezo)

import Html exposing (Html, Attribute, div, img, ul, li, input, text, node)
import Element exposing (show)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
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
  , ingredients : List Ingredient }

defaultModel : Model
defaultModel =
  { search         = ""
  , ingredientsUrls = [ "" ]
  , ingredients     = [ turkey, rijst, pezo ] }

init : String -> (Model, Cmd Msg)
init topic = (defaultModel, fetchImages topic)

toHtml : Ingredient -> Html Msg
toHtml ing = img [ src ing.img, alt ing.name ] []

-- UPDATE

type Msg = TextInput String
         | NewImages (Result Http.Error (List String))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TextInput newContent ->
      ({ model | search = newContent }, fetchImages newContent)

    NewImages (Ok newUrls) ->
      ({ model | ingredientsUrls = newUrls }, Cmd.none)

    NewImages (Err _) ->
      ({ model | ingredientsUrls = ["error"] }, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ stylesheet
    , input [ placeholder "Search for ingredients", onInput TextInput ] []
    , ul [] (List.map (\a -> img [src a, alt a] []) model.ingredientsUrls)
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
stylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            --, attribute "href"      "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
            , attribute "href"      "css/main.css"
            ]
        children = []
    in 
        node tag attrs children
