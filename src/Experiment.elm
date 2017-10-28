module Experiment exposing (..)

import AnimationState exposing (..)
import Ease
import Animation exposing (Animation)
import AnimationFrame
import Time exposing (Time)
import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Direction
    = Forward
    | Backward


toggleDirection : Direction -> Direction
toggleDirection direction =
    case direction of
        Forward ->
            Backward

        Backward ->
            Forward


type alias Model =
    { now : Time
    , animation : AnimationState
    , direction : Direction
    , loop : Bool
    }


type Msg
    = Frame Time
    | Animate
    | Loop Bool


init : ( Model, Cmd Msg )
init =
    pure
        { now = 0
        , animation = Pending
        , direction = Forward
        , loop = False
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.animation of
        Playing _ ->
            AnimationFrame.diffs Frame

        _ ->
            Sub.none


pure : Model -> ( Model, Cmd a )
pure model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loop loop ->
            pure { model | loop = loop }

        Animate ->
            pure (animate model)

        Frame diff ->
            let
                done =
                    isDone animation

                animation =
                    AnimationState.progress model.animation model.now

                now =
                    if done then
                        0
                    else
                        model.now + diff

                next =
                    if done && model.loop then
                        animate
                    else
                        identity
            in
                pure
                    (next
                        { model
                            | now = now
                            , animation = animation
                        }
                    )


animate : Model -> Model
animate model =
    { model
        | animation =
            Playing
                (Animation.animation 0
                    |> Animation.ease Ease.linear
                    |> Animation.duration
                        (1 * Time.second)
                )
        , direction =
            if isDone model.animation then
                toggleDirection model.direction
            else
                model.direction
    }


viewBox : Float -> Css.Color -> Html Msg
viewBox t color =
    div
        [ style
            (asPairs
                [ Css.width (px 30)
                , Css.height (px 30)
                , backgroundColor color
                , position absolute
                , left (px (t * 300))
                ]
            )
        ]
        []


viewA : Float -> Html Msg
viewA t =
    viewBox t (hex "ff0000")


viewB : Float -> Html Msg
viewB t =
    viewBox (1 - t) (hex "00ff00")


viewButton : Html Msg
viewButton =
    button [ onClick Animate ] [ Html.text "Animate" ]


viewCheckbox : Bool -> Html Msg
viewCheckbox loop =
    label []
        [ input [ type_ "checkbox", Html.Attributes.checked loop, onCheck Loop ] []
        , Html.text "Loop"
        ]


wrapper : Model -> List (Html Msg) -> Html Msg
wrapper { loop } content =
    div
        [ style
            (asPairs
                [ Css.width (px 640)
                , margin2 (Css.em 3) auto
                ]
            )
        ]
        [ div [ style (asPairs [ Css.marginBottom (Css.em 1) ]) ] [ viewButton, viewCheckbox loop ]
        , div [ style (asPairs [ position relative ]) ]
            content
        ]


fromToOfDirection : Direction -> ( Float, Float )
fromToOfDirection direction =
    case direction of
        Forward ->
            ( 0, 1 )

        Backward ->
            ( 1, 0 )


view : Model -> Html Msg
view model =
    let
        { now, animation, direction } =
            model

        ( from, to ) =
            fromToOfDirection direction
    in
        case animation of
            Pending ->
                wrapper model [ viewA from, viewB from ]

            Playing animation ->
                let
                    t =
                        Animation.animate now
                            (animation
                                |> Animation.from from
                                |> Animation.to to
                            )
                in
                    wrapper model [ viewA t, viewB t ]

            Done ->
                wrapper model [ viewA to, viewB to ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
