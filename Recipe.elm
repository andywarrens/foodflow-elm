module Recipe exposing (..)

import Html exposing (text, div, p)
import Html.Attributes exposing (class)
import Element exposing (toHtml, show, flow, right, down)
import Collage exposing (scale, move, collage, toForm, groupTransform, square, outlined, defaultLine)

import List exposing (singleton, map)

import MergeStep exposing (MergeStep)

type alias Recipe = 
    { steps : List MergeStep }

defaultRecipe : Recipe
defaultRecipe = { steps = [ MergeStep.defaultMergeStep ] }

main = toHtml <| show defaultRecipe
