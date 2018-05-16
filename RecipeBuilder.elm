module RecipeBuilder exposing (..)

import Recipe exposing (Recipe, addStep, moveStep, changeStep, substep1a, easySalad, saladeKip, RecipeList(..))
import Step exposing (Step, toForm)
import Ingredient exposing (Ingredient, emptyIngredient
                            , turkey, oliveoil, pezo, artisjokhart, zongedroogdtomaten, littlegem, macadamia, rijst)

import Util exposing (Url, stylesheet, stylesheetcdn, initialBackground, size, cols, rows, tupleMap, createOutline)

import Html exposing (Html, text, div, h1, h2, ul, li, a, input, img, hr, button)
import Html.Attributes exposing (placeholder, style, class, alt, src, value)
import Html.Events exposing (onInput, onClick, onMouseEnter, onMouseLeave, onMouseOver, onMouseDown, onMouseUp)
import Element exposing (toHtml, show, right, leftAligned)
import Text exposing (fromString)
import Collage exposing (LineCap(..), collage, moveX)
import Color exposing (Color)

import Http
import Json.Decode as Decode

import List exposing (singleton, map)
import Tuple exposing (first)
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
  { search              : String
  , searchedIngredients : Either String (List Ingredient)
  , hoverIngredient     : Maybe Ingredient
  , selectedStep        : Maybe Step
  , currentRecipe       : Recipe  
  , drag                : DraggableState }

defaultDrag : DraggableState
defaultDrag = { start = Nothing }

defaultModel : String -> Model
defaultModel searchTopic =
  { search              = searchTopic
  , searchedIngredients = Either.Right []
  , hoverIngredient     = Nothing
  , selectedStep        = Just substep1a
  , currentRecipe       = easySalad
  , drag                = defaultDrag }

type alias Draggable =
    { step : Step
    , pos  : Int }
type alias DraggableState = 
    { start : Maybe Draggable }
    

-- UPDATE
----------------------------------------
type Msg = SearchboxEvent SearchboxMsg
         | SelectRecipe Recipe
         | AddStep
         | SelectStep Step
         | AddIngredient
         | DragStart Draggable
         | DragOver Draggable
         | DragEnd
type SearchboxMsg = TextInput String
         | NewImages (Result Http.Error (List String))
         | HoverIngredient (Maybe Ingredient)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectStep step -> 
        ({ model | selectedStep = Just step }, Cmd.none)
    SelectRecipe recipe ->
        ({ model | currentRecipe = recipe}, Cmd.none)
    AddStep ->
        let recipeList = model.currentRecipe.recipe 
            currentRecipe = model.currentRecipe
            newRecipeList = addStep currentRecipe.recipe { action= "test", ingredients= [] }
            newRecipe = { currentRecipe | recipe = newRecipeList }
        in ({ model | currentRecipe = newRecipe}, Cmd.none)
    AddIngredient ->
        case model.selectedStep of
          Just selectedStep ->
            case model.hoverIngredient of
              Just hoverIngredient ->
                let newIngredients = List.append selectedStep.ingredients [hoverIngredient]
                    newStep = { selectedStep | ingredients = newIngredients }
                    recipeList = model.currentRecipe.recipe 
                    newRecipeList = Recipe.changeStep recipeList selectedStep newStep
                    currentRecipe = model.currentRecipe
                    newRecipe = { currentRecipe | recipe = newRecipeList }
                in ({ model | currentRecipe = newRecipe, selectedStep = Just newStep }, Cmd.none)
              Nothing -> (model, Cmd.none)
          Nothing -> (model, Cmd.none)
    DragStart drag -> 
        let draggable = model.drag
            newDraggable = { draggable | start = Just drag }
        in ({ model | drag = newDraggable
                    , selectedStep = Just drag.step }, Cmd.none)
    DragOver end ->
        let recipeList = model.currentRecipe.recipe 
            currentRecipe = model.currentRecipe
            newRecipeList = case model.drag.start of 
                Just begin -> if (end.pos /= begin.pos)
                    then Recipe.moveStep recipeList begin.pos end.pos 
                    else recipeList
                Nothing   -> recipeList
            newRecipe = { currentRecipe | recipe = newRecipeList }
            newDrag = Maybe.map (\a -> { a | pos = end.pos }) model.drag.start
        in ({ model | currentRecipe = newRecipe, drag = DraggableState newDrag  }, Cmd.none)
    DragEnd -> ({ model | drag = defaultDrag }, Cmd.none)
    SearchboxEvent evt -> case evt of
        TextInput newContent ->
          ({ model | search = newContent }, fetchImages newContent)

        NewImages (Ok newUrls) ->
            let urlToIngredient = Ingredient model.search 1 "g"
                ingredients = List.map urlToIngredient newUrls
            in ({ model | searchedIngredients = Either.Right ingredients }, Cmd.none)

        NewImages (Err _) ->
          ({ model | searchedIngredients = Either.Left "error while fetching" }, Cmd.none)

        HoverIngredient ingr ->
          ({ model | hoverIngredient = ingr }, Cmd.none)

-- VIEW
----------------------------------------
view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ stylesheetcdn "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
        "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" 
        "anonymous"
    , stylesheet "css/main.css"
    , div [ class "row" ]
      [ div [ class "col-md-12" ] 
        [ h1 [] [ text "Recipe builder: " ]
        , input [ value model.currentRecipe.name ] [] ] ] 
    , div [ class "row" ]
      [ div [ class "col-md-8" ] 
        [ h2 [] [ text "Edit:" ]
        , editStepView model 
        , h2 [] [ text "Steps:" ] 
        , stepsView model ]
      , div [ class "col-md-3 offset-md-1" ] 
        [ h2 [] [ text "Search:" ]
        , input [ placeholder "Search for ingredients"
                , onInput (SearchboxEvent << TextInput)
                , value model.search 
                , class "form-control" ] []
        , ul [] <| Either.unpack 
                    (\err -> [li [] [ text ("error fetching urls: " ++ err) ]])
                    (map <| ingredientToHtml model.hoverIngredient) 
                    model.searchedIngredients
        ]
      ]
    , hr [] []
    , div [ class "row" ] 
      [ div [ class "col-md-3" ] 
        [ h2 [] [ text "Recipes:" ] 
        , ul [] 
            (List.map (\recipe -> li [] [ a [onClick <| SelectRecipe recipe] [text << .name <| recipe] ])
              [ easySalad, saladeKip, Recipe.defaultRecipe ])
        ]
      ]
    ]
editStepView : Model -> Html Msg
editStepView { currentRecipe } =
      li [] [ button [onClick AddStep] [ text "Add a step"] ]

stepsView : Model -> Html Msg
stepsView { selectedStep, currentRecipe, drag } =
  let createHtml = Step.toForm
        >> (\(form, width) -> collage (round width) size [form])
        >> toHtml
      calculatePosition = List.length >> toFloat >> (\a -> 5+a*0.8*(toFloat size)) >> toString
      createLi pos step = 
          let position  = [("left", (calculatePosition step.ingredients) ++ "px")]
              emptySlot = div [style position, class "empty-slot"] [toHtml <| collage size size [emptyIngredient]]
              draggable = Draggable step pos
              events = [ onMouseDown (DragStart draggable)
                       , onMouseEnter (DragOver draggable)
                       , onMouseUp (DragEnd)
                       , onClick (SelectStep step)]
          in if (selectedStep == Just step) 
            then li (class "selected" :: events) [createHtml step, emptySlot]
            else li events [createHtml step]
      stepsLi = currentRecipe.recipe |> Recipe.getSteps >> List.indexedMap createLi
  in ul [class "no-list"] stepsLi

ingredientToHtml : Maybe Ingredient -> Ingredient -> Html Msg
ingredientToHtml sel ingr = 
  li [onClick AddIngredient] [
    img [ src ingr.img
       , alt ingr.name
       , class (if (sel == Just ingr) then "selected-image" else "")
       , onMouseEnter (SearchboxEvent <| HoverIngredient (Just ingr))
       , onMouseLeave (SearchboxEvent <| HoverIngredient Nothing)
       ] []
    ]

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

