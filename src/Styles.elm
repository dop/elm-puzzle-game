module Styles exposing (..)

import Html exposing (Html)
import Html.Attributes
import Css exposing (..)
import Css.Colors as Colors
import Css.File
import Color.Mixing as Mixing
import Html.CssHelpers exposing (withNamespace, style)


type Classes
    = Button
    | StatusBarItem


type Ids
    = Container
    | Wrapper
    | StatusBar
    | PuzzleWrapper


styles : Html msg
styles =
    let
        { css } =
            Css.File.compile [ puzzleStylesheet ]
    in
        style css


puzzleStylesheet : Stylesheet
puzzleStylesheet =
    stylesheet
        [ id Container
            [ position absolute
            , left zero
            , top zero
            , right zero
            , bottom zero
            , backgroundColor (hex "000000")
            , textAlign center
            , color (hex "FFFFFF")
            ]
        , id Wrapper
            [ margin2 zero auto ]
        , id StatusBar
            [ displayFlex
            , justifyContent spaceBetween
            , padding2 (em 1) zero
            ]
        , class StatusBarItem
            [ fontSize (px 16)
            , fontWeight (int 200)
            , color (rgb 161 171 188)
            ]
        , id PuzzleWrapper
            [ position relative
            , margin2 zero auto
            , backgroundColor Colors.black
            ]
        , class Button
            [ fontSize (px 15)
            , color (rgb 255 255 255)
            , backgroundColor (rgb 35 108 229)
            , padding2 (px 12) (px 18)
            , margin2 (em 2) zero
            , border3 (px 1) solid (rgb 89 146 241)
            , borderRadius (px 12)
            ]
        ]


wrapper : Int -> List ( String, String )
wrapper width =
    asPairs
        [ Css.width (toFloat width |> px) ]


puzzle : ( Int, Int ) -> List ( String, String )
puzzle ( width, height ) =
    asPairs
        [ Css.width (toFloat width |> px)
        , Css.height (toFloat height |> px)
        ]
