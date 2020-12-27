module Msg exposing (..)

import Recipe exposing (Recipe)
import Step exposing (Step)
import Ingredient exposing (Ingredient)

import Http

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

type alias Draggable =
    { step : Step
    , pos  : Int }
type DraggableState = NotDragging | Dragging Draggable
