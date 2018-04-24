module RecipeBuilder exposing (..)

import Recipe exposing (Recipe, saladeKip)
import Step exposing (Step, toForm)
import Ingredient exposing (Ingredient
                            , turkey, oliveoil, pezo, artisjokhart, zongedroogdtomaten, littlegem, macadamia, rijst)

import Util exposing (Url, stylesheet, stylesheetcdn, initialBackground, size, cols, rows, tupleMap)

import Html exposing (Html, text, div, h2, ul, li, a, input, img, hr)
import Html.Attributes exposing (placeholder, class, alt, src, value)
import Html.Events exposing (onInput, onClick, onMouseEnter, onMouseLeave, onMouseOver)
import Element exposing (toHtml, show, right, leftAligned)
import Text exposing (fromString)
import Collage exposing (LineCap(..), collage, moveX)
import Color exposing (Color)

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
        ({ model | selectedStep = Just step }, Cmd.none)
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
  let offsetIngr = -100---size |> toFloat
      makeSquare = \color -> Collage.square 
        >> Collage.outlined (Collage.solid color)
        >> Collage.scale 0.5
      makeDot = \color -> Collage.circle >> Collage.filled color
      square = [makeSquare Color.blue 100, makeDot Color.blue 20]
          |> List.map (Collage.moveX offsetIngr)
          |> Collage.group 
      newSquare = Collage.group [makeSquare Color.red 100, makeDot Color.red 5, square]

      substep2b = { ingredients = [ zongedroogdtomaten, littlegem ]
                  , action = "Hak in stukjes"}
       |> Step.toForm >> Tuple.first -->> moveX offsetIngr
       >> (flip (::)) [initialBackground] >> List.reverse
  in div [ class "container" ]
    [ stylesheetcdn "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
        "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" 
        "anonymous"
    , stylesheet "css/main.css"
    , collage 400 200 substep2b |> toHtml 
    , collage 400 200 [newSquare] |> toHtml ]
--    , div [ class "row" ]
--      [ div [ class "col-md-8" ] 
--        [ h2 [] [ text "Recipe builder: ", input [ value model.selectedRecipe.name ] [] ]
--        , screenHtml ]
--      , div [ class "col-md-3 offset-md-1" ] 
--        [ h2 [] [ text "Search:" ]
--        , input [ placeholder "Search for ingredients"
--                , onInput (SearchboxEvent << TextInput)
--                , value model.search 
--                , class "form-control" ] []
--        , ul [] (ingredientUrlsToHtml model.hoverIngredient model.ingredientsUrls) ]
--      ]
--    , div [ class "row" ] 
--      [ div [ class "col-md-3" ] 
--        [ h2 [] [ text "Steps:" ] 
--        , buildStepsView model.selectedRecipe model.selectedStep ]
--      ]
--    , hr [] []
--    , div [ class "row" ] 
--      [ div [ class "col-md-3" ] 
--        [ h2 [] [ text "Recipes:" ] 
--        , ul [] 
--            (List.map (\recipe -> li [] [ a [onClick <| SelectRecipe recipe] [text << .name <| recipe] ])
--              [ saladeKip, Recipe.defaultRecipe ])
--        ]
--      ]
--    ]

buildStepsView : Recipe -> Maybe Step -> Html Msg
buildStepsView { recipe } step = 
    let steps = Recipe.getSteps recipe
    |> List.map (\a -> li [class (if step == Just a then "selected-image" else "")]
                          [.action >> text <| a])
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

