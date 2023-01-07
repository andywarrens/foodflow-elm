module Ingredient exposing (Ingredient, ingredients, toForm, emptyIngredient,
                            turkey, oliveoil, pezo, artisjokhart, zongedroogdtomaten, 
                            littlegem, macadamia, rijst, onion, tomato, pan)

import Util 

import Html exposing (text, div, p)
import Html.Attributes exposing (class)
import List exposing (singleton, map)

import Collage exposing (Collage, image, group, outlined, shift, uniform)
import Collage exposing (square, rectangle)
import Collage exposing (filled)
import Color exposing (gray)


type alias Ingredient = 
    { name : String
    , qty  : Float
    , unit : String
    , img  : String }

toForm : Ingredient -> Collage msg
toForm { img } =
    image (toFloat Util.size, toFloat Util.size) img 

addIngredient : (Float, Float) -> Ingredient -> Collage msg -> Collage msg
addIngredient position ingredient board =
    let
        (dx, dy) = Util.calculateMove position
        blockForm = toForm ingredient |> shift (dx, dy)
    in group [board, blockForm]

emptyIngredient : Collage msg
emptyIngredient = 
    let tileSize = toFloat Util.size
        scale = 0.6
        scaleMargin = 0.5 * (1-scale)*tileSize
        transform = Collage.scale scale >> shift (-scaleMargin, scaleMargin/2)
        greyLine = Collage.dash (toFloat 1) (uniform gray)
        graySquare = outlined greyLine (square 100)
        rect w h = filled (uniform gray) (rectangle w h)
        thick = 2
        cross  = group [ rect 20 thick, rect thick 20 ]
    in group [ cross, graySquare ] |> transform
        
--- Example ingredients
turkey              = Ingredient "Turkey" 220 "gram" "img/turkey.png"
oliveoil            = Ingredient "Olive Oil" 1 "el" "img/olijfolie.jpg"
pezo                = Ingredient "Salt & Pepper" 1 "snuifje" "img/pezo.jpg"
artisjokhart        = Ingredient "Artisjokhart" 3 "stuks" "img/artisjokhart.jpg"
zongedroogdtomaten  = Ingredient "Zongedroogde tomaten" 4 "stuks" "img/zongedroogd-tomaten.jpg"
littlegem           = Ingredient "Little gem" 1 "kropje" "img/littlegem.jpg"
macadamia           = Ingredient "Macadamia noten" 1 "handvol" "img/macadamia.jpg"
rijst               = Ingredient "Rice" 1 "zakje" "img/rice.png"
onion               = Ingredient "Onion" 1 "piece" "img/onion.png"
tomato              = Ingredient "Tomato" 4 "piece" "img/tomato.png"
pan                 = Ingredient "Pan" 1 "piece" "img/pan.png"

ingredients : List Ingredient
ingredients = 
    [ tomato
    , onion
    , turkey
    ]
