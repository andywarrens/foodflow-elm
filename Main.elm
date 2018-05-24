port module Main exposing (..)

import Recipe exposing (Recipe, drawRecipe, defaultRecipe)
import Ingredient exposing (Ingredient)
import Encoder exposing (recipeJson, decodeRecipe)

import Util exposing (tupleMap, stylesheet, stylesheetcdn, initialBackground,
                      size, cols, rows, Url)

import Html exposing (Html, text, div, h2, ul, li, a, input, img, i, hr, button)
import Html.Attributes exposing (placeholder, class, alt, src, value, type_, style)
import Html.Events exposing (onInput, onClick, onMouseOver)
import Element exposing (toHtml, show, flow, right, down)
import Collage exposing (Form, move, moveY, collage, toForm, group, solid, square, rect, circle, outlined)
import Color exposing (red, blue)

import Http
import Json.Encode as Encode
import Json.Decode as Decode

import List exposing (singleton, map)

import LocalStorage exposing (LocalStorage, setPorts)
import LocalStorage.SharedTypes exposing (Ports, Value, Key, Operation(..), receiveWrapper
                                         ,ClearPort, GetItemPort, ListKeysPort, ReceiveItemPort, SetItemPort
                                         )

port getItem     : GetItemPort msg
port setItem     : SetItemPort msg
port clear       : ClearPort msg
port listKeys    : ListKeysPort msg
port receiveItem : ReceiveItemPort msg

getPorts : Model -> Ports Msg
getPorts model =
    LocalStorage.getPorts model.storage

main =
    Html.programWithFlags
    { init = \initialModel -> init initialModel (LocalStorage.makeRealPorts getItem setItem clear listKeys)
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
  , recipeDB           : List Recipe
  , selectedSubRecipe  : Maybe Recipe
  , searchboxState     : PanelVisibility
  , storage            : LocalStorage Msg }

init : Value -> Ports Msg -> ( Model, Cmd Msg )
init initialModel ports =
    defaultModel "ananas" (LocalStorage.make ports "foodflow-app") ! []

defaultModel : String -> LocalStorage Msg -> Model
defaultModel searchTopic localStorage =
  { search             = searchTopic 
  , ingredientsUrls    = []
  , selectedIngredient = Nothing
  , selectedRecipe     = defaultRecipe  
  , recipeDB           = [ defaultRecipe ]
  , selectedSubRecipe  = Nothing 
  , searchboxState     = Closed
  , storage            = localStorage }

type PanelVisibility = Open | Closed

-- UPDATE
----------------------------------------
type Msg = RecipeMsg String -- dummy for test
         | SearchboxEvent SearchboxMsg
         | HoverSubRecipe Recipe
         | SelectRecipe Recipe
         | SaveRecipe
         | LoadRecipes
         | UpdatePorts Operation (Maybe (Ports Msg)) Key Value
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
        model ! [ LocalStorage.setItem model.storage "recipe2" (recipeJson model.selectedRecipe) ]
    LoadRecipes -> 
        model ! [ LocalStorage.getItem model.storage "recipe" ]
    UpdatePorts operation ports key value ->
        updatePortsHelper operation ports key value model
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
        ToggleSearchbox ->
            ({ model | searchboxState = if model.searchboxState == Open then Closed else Open }, Cmd.none)

updatePortsHelper operation ports key jsonRecipe model = 
    let mdl = case operation of
        GetItemOperation -> let conversion = decodeRecipe jsonRecipe
                            in case conversion of
                              Err msg -> let _ = Debug.log "conversion-error" msg
                                         in model
                              Ok recipe -> { model | recipeDB = [recipe] }
        _ -> model
    in { mdl | storage = case ports of
                Nothing -> model.storage
                Just ps -> setPorts ps model.storage } ! []

-- VIEW
----------------------------------------
view : Model -> Html Msg
view model =
  let screen = drawRecipe (0, 5) model.selectedRecipe model.selectedSubRecipe initialBackground
      width = round <| cols*(toFloat size)
      height = round <| rows*(toFloat size)
      screenHtml = toHtml <| collage width height [screen] 
      searchBoxPanel = 
        let header = [ text "Search:"
                     , button [ onClick (SearchboxEvent ToggleSearchbox)
                              , type_ "button"
                              , class "btn btn-secondary"
                              , style [("position", "absolute")
                                      ,("right", "0")] ]
                              [ text <| searchboxStateToText model.searchboxState ] ]
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
subscriptions model = 
    receiveItem <| receiveWrapper UpdatePorts "recipe"
