module Button
    exposing
        ( init
        , ViewConfig
        , update
        , subscriptions
        , view
        , Model
        , Msg
        , getParentMsg
        , reset
        )

import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias ActiveModel a =
    { hover : Bool, label : String, msg : a }


type Model a
    = Active (ActiveModel a)
    | Disabled { label : String }


type Msg a
    = Emit a
    | MouseEnter
    | MouseLeave


type alias ViewConfig =
    { activeColor : Color
    , defaultColor : Color
    , disabledColor : Color
    }


getParentMsg : Msg a -> Maybe a
getParentMsg msg =
    case msg of
        Emit msg ->
            Just msg

        _ ->
            Nothing


reset : Model a -> Model a
reset model =
    case model of
        Disabled _ ->
            model

        Active model ->
            Active { model | hover = False }


disabled : String -> Model Never
disabled label =
    Disabled { label = label }


init : a -> String -> Model a
init msg label =
    Active { hover = False, label = label, msg = msg }


update : Msg a -> Model a -> Model a
update msg model =
    case msg of
        Emit _ ->
            model

        MouseEnter ->
            updateActive model (\model -> { model | hover = True })

        MouseLeave ->
            updateActive model (\model -> { model | hover = False })


updateActive : Model a -> (ActiveModel a -> ActiveModel a) -> Model a
updateActive model f =
    case model of
        Active model ->
            Active (f model)

        _ ->
            model


subscriptions : Model a -> Sub Msg
subscriptions model =
    Sub.none


view : ViewConfig -> Model a -> Html (Msg a)
view config model =
    let
        commonStyle =
            [ fontSize (px 16)
            , fontFamilies [ qt "Press Start 2P" ]
            , textTransform uppercase
            , color (rgb 255 255 255)
            , padding2 (Css.em 0.75) (Css.em 2)
            , border zero
            , borderRadius (px 3)
            , margin zero
            , boxShadow4 (px 0) (px 0) (px 4) (rgb 0 0 0)
            , textShadow4 (px 0) (px 1) (px 2) (rgb 0 0 0)
            ]
    in
        case model of
            Active { hover, label, msg } ->
                let
                    styles =
                        [ backgroundColor
                            (if hover then
                                config.activeColor
                             else
                                config.defaultColor
                            )
                        , cursor pointer
                        ]
                            ++ commonStyle
                in
                    Html.button
                        [ style (asPairs styles)
                        , onClick (Emit msg)
                        , onMouseEnter MouseEnter
                        , onMouseLeave MouseLeave
                        ]
                        [ Html.text label ]

            Disabled { label } ->
                let
                    styles =
                        [ backgroundColor config.disabledColor ]
                            ++ commonStyle
                in
                    Html.button
                        [ style (asPairs styles) ]
                        [ Html.text label ]
