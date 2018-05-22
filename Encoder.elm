module Encoder exposing (..)

import Recipe exposing (Recipe, defaultRecipe)
import Step exposing (Step, defaultStep)
import Ingredient exposing (Ingredient, turkey)

import Html exposing (..)
import Json.Encode as Encoder exposing (..)
import Json.Decode as Decoder exposing (..)

main =
  Html.program
    { init = ("", Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model = String
type Msg = NoOp

view : Model -> Html Msg
view model = [ text << toString <| turkey     -- show Elm object
             , ingredientJson turkey          -- encode Elm object to JSON
                |> decodeIngredient           -- decode JSON to Elm object
                |> toString
                |> Html.text
             , text << toString <| defaultStep -- show Elm object
             , stepJson defaultStep            -- encode Elm object to JSON
                |> decodeStep                  -- decode JSON to Elm object
                |> toString
                |> Html.text 
             , text << toString <| recipeJson defaultRecipe -- encode Elm object to JSON
             , recipeJson defaultRecipe          -- encode Elm object to JSON
                |> decodeRecipe                  -- decode JSON to Elm object
                |> toString
                |> Html.text ]

  |> List.map (\html -> p [] [html])
  |> div []


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

decodeField : String -> Decoder.Decoder a -> (model -> a -> model)
    -> Decoder.Value -> Result String model -> Result String model
decodeField fieldName decoder setter value model =
  let decoded = Decoder.decodeValue (Decoder.field fieldName decoder) value
  in case model of
    Err s ->
      case decoded of
        Err newMessage ->
            Err (s ++ "\nAnd " ++ newMessage)
        Ok _ ->
            model
    Ok v ->
      Result.map (setter v) decoded

---- Ingredient
ingredientJson : Ingredient -> Encoder.Value
ingredientJson ingr =
    Encoder.object [ ("name", Encoder.string ingr.name)
                   , ("qty", Encoder.float ingr.qty)
                   , ("unit", Encoder.string ingr.unit)
                   , ("img", Encoder.string ingr.img) ]

decodeIngredient : Encoder.Value -> Result String Ingredient
decodeIngredient value =
  let
      setName model value = { model | name = value }
      setQty model value  = { model | qty  = value }
      setUnit model value = { model | unit = value }
      setImg model value  = { model | img  = value }
  in Ok { name = "", qty = 0, unit = "", img = "" } 
    |> decodeField "name" Decoder.string setName value
    |> decodeField "qty" Decoder.float setQty value
    |> decodeField "unit" Decoder.string setUnit value
    |> decodeField "img" Decoder.string setImg value

ingredientDecoder : Decoder.Decoder Ingredient
ingredientDecoder =
  Decoder.map4 Ingredient
    (field "name" Decoder.string)
    (field "qty"  Decoder.float)
    (field "unit" Decoder.string)
    (field "img"  Decoder.string)

---- Step
stepJson : Step -> Encoder.Value
stepJson step =
    let encodeIngredients : List Ingredient -> Encoder.Value
        encodeIngredients = List.map ingredientJson >> Encoder.list
    in Encoder.object [ ("ingredients", encodeIngredients step.ingredients)
                      , ("action", Encoder.string step.action) ]

decodeStep : Encoder.Value -> Result String Step
decodeStep = decodeValue stepDecoder

stepDecoder : Decoder.Decoder Step
stepDecoder =
  Decoder.map2 Step
    (field "ingredients" (Decoder.list ingredientDecoder))
    (field "action" Decoder.string)

---- Recipe
recipeJson : Recipe -> Encoder.Value
recipeJson recipe =
    let encodeRecipeList : Recipe.RecipeList -> Encoder.Value
        encodeRecipeList l = case l of
          Recipe.End -> Encoder.object 
            [ ("type", Encoder.string "end") ]
          Recipe.Node step xs -> Encoder.object
            [ ("type", Encoder.string "step")
            , ("step", stepJson step)
            , ("rest", encodeRecipeList xs) ]
          Recipe.Merge left right -> Encoder.object 
            [ ("type", Encoder.string "merge")
            , ("left", recipeJson left)
            , ("right", recipeJson right) ]
    in Encoder.object [ ("recipe", encodeRecipeList recipe.recipe)
                      , ("name", Encoder.string recipe.name) 
                      , ("comments", Encoder.string recipe.comments) ]

decodeRecipeList : Decoder.Decoder Recipe.RecipeList
decodeRecipeList =
    let
        convert : String -> Decoder.Decoder Recipe.RecipeList
        convert recipelist =
            case recipelist of
                "end"   -> Decoder.succeed (Recipe.End)
                "step" -> Decoder.map2 Recipe.Node
                  (field "step" stepDecoder)
                  (field "rest" decodeRecipeList)
                "merge" -> Decoder.map2 Recipe.Merge
                  (field "left" recipeDecoder)
                  (field "right" recipeDecoder)
                _ -> Decoder.fail <| "Unknown value to decode: " ++ recipelist
    in
        (Decoder.at ["type"] Decoder.string) |> Decoder.andThen convert

decodeRecipe : Encoder.Value -> Result String Recipe
decodeRecipe = decodeValue recipeDecoder

recipeDecoder : Decoder.Decoder Recipe
recipeDecoder =
  Decoder.map3 Recipe
    (field "recipe" decodeRecipeList)
    (field "name" Decoder.string)
    (field "comments" Decoder.string)
