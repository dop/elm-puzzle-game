module Credits exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Css exposing (..)


h : String -> Html a
h text =
    h2 [] [ Html.text text ]


p : String -> Html a
p text =
    Html.p [ style (asPairs [ textIndent (Css.em 1) ]) ]
        [ Html.text text ]


view : Html a
view =
    div [ style (asPairs [ textAlign left ]) ]
        [ h "Thanks"
        , p "To my wife Dovilė, who wholeheartly supported development of this app."
        , p "To my daughter, who constantly interrupted with much needed play time."
        , p "To Elm, for allowing me to stay sane."
        , h "Resources"
        , p ""
        ]
