module MergeStep exposing (MergeStep, defaultMergeStep)

import Step exposing (..)

import Element exposing (toHtml, show)


type alias MergeStep = 
    { steps : List Step
    , action : String }

defaultMergeStep : MergeStep
defaultMergeStep = 
    { steps = 
        [ Step.defaultStep
        , { ingredients =
              [ { name = "Spaghetti", qty = 500, unit = "gram" }
              , { name = "Water", qty = 2, unit = "litre" } ]
          , action = "Boil" } ]
    , action = "Drain spaghetti and put everything together on a plate" }

main = toHtml <| show defaultMergeStep

