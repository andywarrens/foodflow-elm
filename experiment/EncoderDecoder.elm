module Encoder exposing (..)

import Recipe exposing (Recipe)
import Step exposing (Step)
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
view model = [ exampleObject                  -- JSON Object
                |> decodeModel                -- decode JSON to Elm
                |> toString
                |> Html.text 
             , text << toString <| defaultFoo -- show Elm object
             , fooJson defaultFoo             -- encode Elm object to JSON
                |> decodePlayer               -- decode JSON to Elm object
                |> toString
                |> Html.text 
             , text << toString <| fooJson2   -- show JSON Object
             , fooJson2                       -- JSON Object
                |> decodePlayer               -- decode JSON to Elm 
                |> toString
                |> Html.text ]
  |> List.map (\html -> p [] [html])
  |> div []


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- Decode
----------------------------------------
exampleObject = 
  Encoder.object 
    [ ("name", Encoder.string "noah")
    , ("age", Encoder.int 10) 
    ]

type alias DecoderModel = { name : String , age : Int }
setAge : DecoderModel -> Int -> DecoderModel
setAge model value = { model | age = value }
setName : DecoderModel -> String -> DecoderModel
setName model name = { model | name = name }

decodeModel : Encoder.Value -> Result String DecoderModel
decodeModel value =
  Ok { name = "", age = 0 } 
    |> decodeField "age" Decoder.int setAge value
    |> decodeField "name" Decoder.string setName value

decodeField :
    String
    -> Decoder.Decoder a
    -> (model -> a -> model)
    -> Decoder.Value
    -> Result String model
    -> Result String model
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


-- Encode
----------------------------------------
type alias Player = { greeting : Greeting, age : Int }
type alias Greeting = { text : GreetingText }
type GreetingText = Hello | Goodbye

defaultFoo : Player
defaultFoo = { greeting = (Greeting Goodbye), age = 15 }

fooJson : Player -> Encoder.Value
fooJson foo =
    Encoder.object [ ("greeting", barJson foo.greeting)
                   , ("age", Encoder.int foo.age) ]

fooJson2 : Encoder.Value
fooJson2 =
    Encoder.object [ ("greeting", Encoder.object [ ("text", Encoder.string "test") ])
                   , ("age", Encoder.int 42) ]

barJson : Greeting -> Encoder.Value
barJson bar =
    Encoder.object [ ("text", bazJson bar.text) ]

bazJson : GreetingText -> Encoder.Value
bazJson baz =
    case baz of
        Hello  -> Encoder.string "Hello"
        Goodbye -> Encoder.string "Goodbye"

-- write a decoder for Foo/Bar/Baz
decodePlayer : Encoder.Value -> Result String Player
decodePlayer value =
  Ok { greeting = (Greeting Hello), age = 0 }  -- give an empty object, because fields can't be added
    |> decodeField "greeting" decodeGreeting setPlayerGreeting value
    |> decodeField "age" Decoder.int setPlayerAge value

setPlayerGreeting model value = { model | greeting = value }
setPlayerAge model value = { model | age = value }

decodeGreeting : Decoder.Decoder Greeting
decodeGreeting =
    let
        convert : String -> Decoder.Decoder Greeting
        convert raw =
            case raw of
                "Hello"   -> Decoder.succeed (Greeting Hello)
                "Goodbye" -> Decoder.succeed (Greeting Goodbye)
                _ -> Decoder.fail <| "Unknown value to decode: " ++ raw
    in
        (Decoder.at ["text"] Decoder.string) |> Decoder.andThen convert

