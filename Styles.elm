module Styles exposing (..)

import Html
import Html.Attributes
import Css exposing (..)
import Css.Colors as Colors
import Color.Mixing as Mixing


type Classes
    = Item


type Ids
    = Header
    | Puzzle
    | PlayAgainButton


toStyle : List Css.Style -> Html.Attribute msg
toStyle =
    Css.asPairs >> Html.Attributes.style


white =
    rgb 255 255 255


playAgainButton =
    toStyle
        [ fontSize (px 15)
        , color (rgb 255 255 255)
        , backgroundColor (rgb 35 108 229)
        , padding2 (px 12) (px 18)
        , margin2 (em 2) zero
        , border3 (px 1) solid (rgb 89 146 241)
        , borderRadius (px 12)
        ]


puzzleWrapper ( width, height ) =
    toStyle
        [ position relative
        , Css.width (px width)
        , Css.height (px height)
        , margin2 zero auto
        , backgroundColor Colors.black

        -- , boxShadow4 zero zero (px 32) Colors.black
        ]


container =
    toStyle
        [ position absolute
        , left zero
        , top zero
        , right zero
        , bottom zero
        , backgroundColor (rgb 73 77 83)
        , textAlign center
        , color white
        ]


wrapper width =
    toStyle
        [ Css.width (px width)
        , margin2 zero auto
        ]


statusBar =
    toStyle
        [ displayFlex
        , justifyContent spaceBetween
        , padding2 (em 1) zero
        ]


statusBarItem =
    toStyle
        [ fontSize (px 22)
        , fontWeight (int 200)
        , color (rgb 161 171 188)
        ]
