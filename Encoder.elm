module Msg exposing (..)

import Recipe exposing (Recipe)
import Step exposing (Step)
import Ingredient exposing (Ingredient, turkey)

import Html exposing (..)
import Json.Encode as Json exposing (..)

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
view model = text <| Json.encode 4 (fooJson defaultFoo)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

type alias Foo = { foo : Bar }
type alias Bar = { bar : Baz }
type Baz = Baz | Quux

-- Decode
----------------------------------------


-- Encode
----------------------------------------
defaultFoo : Foo
defaultFoo = { foo = (Bar Baz) }

fooJson : Foo -> Value
fooJson foo =
    Json.object [ ("Foo", barJson foo.foo) ]

barJson : Bar -> Value
barJson bar =
    Json.object [ ("Bar", bazJson bar.bar) ]

bazJson : Baz -> Value
bazJson baz =
    case baz of
        Baz -> string "Baz"
        Quux -> string "Quux"

