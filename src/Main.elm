module Main exposing (..)

import Recipe exposing (Recipe, drawRecipe, defaultRecipe)
import Ingredient exposing (Ingredient)
--import Encoder exposing (recipeJson, decodeRecipe)

import Util exposing (tupleMap, stylesheet, stylesheetcdn, initialBackground,
                      size, cols, rows, Url)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onMouseOver)
import Url

import Collage exposing (group, circle, rectangle, filled)
import Collage exposing (defaultLineStyle, outlined, uniform)
import Collage exposing (shift, shiftX, shiftY, rendered)

import Collage.Text as Text
import Collage.Text exposing (fromString)
import Collage.Layout exposing (align, left)
import Color exposing (red, blue)

import Http
import Json.Encode as Encode
import Json.Decode as Decode

import List exposing (singleton, map)
import Collage.Render exposing (svg)
import Util exposing (solid)

--import LocalStorage exposing (LocalStorage, setPorts)
--import LocalStorage.SharedTypes exposing (Ports, Value, Key, Operation(..), receiveWrapper
--                                         ,ClearPort, GetItemPort, ListKeysPort, ReceiveItemPort, SetItemPort
--                                         )

--port getItem     : GetItemPort msg
--port setItem     : SetItemPort msg
--port clear       : ClearPort msg
--port listKeys    : ListKeysPort msg
--port receiveItem : ReceiveItemPort msg

main : Program () Model Msg
main =
  Browser.application
    { init = \flags url key -> ( defaultModel "ananas", Cmd.none )
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- MODEL
----------------------------------------
type alias Model =
  { search             : String
  , ingredientsUrls    : List String
  , selectedIngredient : Maybe Ingredient
  , selectedRecipe     : Recipe
  , recipeDB           : List Recipe
  , selectedSubRecipe  : Maybe Recipe
  , searchboxState     : PanelVisibility
  }

defaultModel : String -> Model
defaultModel searchTopic =
  { search             = searchTopic 
  , ingredientsUrls    = []
  , selectedIngredient = Nothing
  , selectedRecipe     = defaultRecipe  
  , recipeDB           = [ defaultRecipe ]
  , selectedSubRecipe  = Nothing 
  , searchboxState     = Closed
  }

type PanelVisibility = Open | Closed

-- UPDATE
----------------------------------------
type Msg = RecipeMsg String -- dummy for test
         | SearchboxEvent SearchboxMsg
         | HoverSubRecipe Recipe
         | SelectRecipe Recipe
         | SaveRecipe
         | LoadRecipes
         | LinkClicked Browser.UrlRequest
         | UrlChanged Url.Url
type SearchboxMsg = TextInput String
         | NewImages (Result Http.Error (List String))
         | SelectIngredient Url
         | ToggleSearchbox

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
    SaveRecipe -> 
        -- model ! [ LocalStorage.setItem model.storage "recipe2" (recipeJson model.selectedRecipe) ]
        ( model, Cmd.none )
    LoadRecipes -> 
        -- model ! [ LocalStorage.getItem model.storage "recipe" ]
        ( model, Cmd.none )
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Cmd.none )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url -> ( model , Cmd.none)
    SearchboxEvent evt -> case evt of
        TextInput newContent ->
          ({ model | search = newContent }, Cmd.none)
          -- ({ model | search = newContent }, fetchImages newContent)

        NewImages (Ok newUrls) ->
          ({ model | ingredientsUrls = newUrls }, Cmd.none)

        NewImages (Err _) ->
          ({ model | ingredientsUrls = ["error"] }, Cmd.none)

        SelectIngredient url ->
            let ingr = Ingredient model.search 1 "piece" url
            in ({ model | selectedIngredient = Just ingr }, Cmd.none)
        ToggleSearchbox ->
            ({ model | searchboxState = if model.searchboxState == Open then Closed else Open }, Cmd.none)


-- VIEW
----------------------------------------
view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body = [ view2 model ]
  }

view2 : Model -> Html Msg
view2 model =
  let screen = drawRecipe (0, 5) model.selectedRecipe model.selectedSubRecipe initialBackground
      screenHtml = svg <| screen
      searchBoxPanel = 
        let header = [ text "Search:"
                     , button [ onClick (SearchboxEvent ToggleSearchbox)
                              , type_ "button"
                              , class "btn btn-secondary"
                              , style "position" "absolute"
                              , style "right" "0"
                              ]
                              [ text <| searchboxStateToText model.searchboxState 
                              ] 
                      ]
            body = [ input [ placeholder "Search for ingredients"
                           , onInput (SearchboxEvent << TextInput)
                           , value model.search 
                           , class "form-control" ] []
                   , ul [] (List.map (li [] << List.singleton << ingredientUrlToHtml model.selectedIngredient) model.ingredientsUrls) ] 
        in case model.searchboxState of
          Open   -> h2 [] (header ++ body)
          Closed -> h2 [] header

  in div [ class "container" ]
    [ stylesheetcdn "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
        "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" 
        "anonymous"
    , stylesheet "css/main.css"
    , div [ class "row" ]
      [ div [ class "col-md-8" ] [ h2 [] [ text "Recipe: ", i [] [ text model.selectedRecipe.name ] ], screenHtml ]
      , div [ class "col-md-3 offset-md-1" ] 
        ([ searchBoxPanel, hr [] [] ] ++ savePanel) ]
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
              model.recipeDB )
        ]
      ]
    ]

savePanel : List (Html Msg)
savePanel =  
  [ button [ onClick SaveRecipe
           , type_ "button" , class "btn btn-info" ]
           [ text "Save this recipe" ] 
  , button [ onClick LoadRecipes
           , type_ "button" , class "btn btn-light" ]
           [ text "Load all recipes" ] ]

searchboxStateToText : PanelVisibility -> String
searchboxStateToText v = case v of
    Open   -> "CLOSE"
    Closed -> "OPEN"

buildSubRecipes : Recipe -> Html Msg
buildSubRecipes { recipe } = 
    let subrecipes = Recipe.getSubRecipes recipe
        createLi subrecipe = li [onMouseOver (HoverSubRecipe subrecipe)] [text <| .name subrecipe]
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
--fetchImages : String -> Cmd Msg
--fetchImages topic =
--  let url = "http://127.0.0.1:8080/?image=" ++ topic
--      request = Http.get url decodeUrls
--  in Http.send (SearchboxEvent << NewImages) request


decodeUrls : Decode.Decoder (List String)
decodeUrls =
  Decode.at ["data", "images"] (Decode.list Decode.string)

-- SUBSCRIPTIONS
----------------------------------------
--subscriptions : Model -> Sub Msg
--subscriptions model = 
--    receiveItem <| receiveWrapper UpdatePorts "recipe"
--
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
