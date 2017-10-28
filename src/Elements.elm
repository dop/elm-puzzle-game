module Elements exposing (..)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)


bigButton_ : Css.Color -> Maybe msg -> String -> Html msg
bigButton_ bgColor msg label =
    let
        attrs =
            case msg of
                Just msg ->
                    [ onClick msg ]

                Nothing ->
                    []

        styles =
            [ fontSize (px 16)
            , fontFamilies [ qt "Press Start 2P" ]
            , textTransform uppercase
            , color (rgb 255 255 255)
            , backgroundColor bgColor
            , padding2 (Css.em 0.75) (Css.em 2)
            , border3 (px 0) solid (rgb 89 146 241)
            , borderRadius (px 3)
            , margin zero
            , boxShadow4 (px 1) (px 2) (px 16) (rgb 0 0 0)
            , textShadow4 zero zero (px 4) (rgb 0 0 0)
            , cursor pointer
            ]
    in
        Html.button
            ([ style (asPairs styles) ] ++ attrs)
            [ Html.text label ]


bigButton : String -> msg -> Html msg
bigButton label msg =
    bigButton_ (hex "32cd32") (Just msg) label


bigButtonDisabled : String -> Html msg
bigButtonDisabled label =
    bigButton_ (hex "gray") Nothing label


startScreen : Html a -> Html a
startScreen html =
    let
        styles =
            [ padding2 (Css.em 3) zero ]
    in
        div [ style (asPairs styles) ]
            [ h1 [] [ Html.text "Puzzl'em!" ]
            , html
            ]


container : Int -> Html a -> Html a
container width html =
    let
        containerStyle =
            [ position absolute
            , left zero
            , top zero
            , right zero
            , bottom zero
            , backgroundColor (hex "202020")
            , textAlign center
            , color (hex "FFFFFF")
            ]

        wrapperStyle =
            [ margin2 zero auto
            , Css.width (px (toFloat width))
            ]
    in
        div [ style (asPairs containerStyle) ]
            [ div [ style (asPairs wrapperStyle) ]
                [ html ]
            ]
