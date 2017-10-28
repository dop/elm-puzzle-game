module Menu
    exposing
        ( init
        , update
        , view
        , subscriptions
        , Model
        , Msg
        , node
        , leaf
        , getParentMsg
        )

import AnimationState exposing (..)
import Animation exposing (Animation)
import AnimationFrame
import Ease
import Time exposing (Time)
import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Button


type alias TransitionData a =
    { time : Time
    , animation : AnimationState
    , from : Screen a
    , to : Screen a
    }


type View a
    = Single (Screen a)
    | Transition (TransitionData a)


type alias Model a =
    { stack : List (Screen a)
    , current : View a
    }


type Msg a
    = Push (Screen a)
    | Pop
    | Emit a
    | ButtonMsg Int (Button.Msg (Msg a))
    | Frame Time


type alias Screen a =
    List ( Int, Node a )


type alias Node a =
    Button.Model (Msg a)


getParentMsg : Msg a -> Maybe a
getParentMsg msg =
    case msg of
        Emit msg ->
            Just msg

        ButtonMsg _ msg ->
            Button.getParentMsg msg
                |> Maybe.andThen getParentMsg

        _ ->
            Nothing


init : List (Node a) -> Model a
init tree =
    { stack = []
    , current = Single (enum tree)
    }


enum : List a -> List ( Int, a )
enum list =
    let
        iter i xs =
            case xs of
                [] ->
                    []

                x :: xs ->
                    ( i, x ) :: iter (i + 1) xs
    in
        iter 0 list


node : String -> List (Node a) -> Node a
node title items =
    Button.init (Push (enum items)) title


leaf : String -> a -> Node a
leaf title msg =
    Button.init (Emit msg) title


map : (( Int, Node a ) -> Node a) -> Model a -> Model a
map f model =
    let
        current =
            case model.current of
                Single screen ->
                    Single (enum (List.map f screen))

                Transition transition ->
                    Transition { transition | to = enum (List.map f transition.to) }
    in
        { model | current = current }


transition : Screen a -> Screen a -> View a
transition from to =
    Transition
        { from = from
        , to = to
        , time = 0
        , animation =
            Playing
                (Animation.animation 0
                    |> Animation.ease Ease.inOutQuad
                    |> Animation.duration (0.4 * Time.second)
                )
        }


update : Msg a -> Model a -> Model a
update msg model =
    case msg of
        Frame diff ->
            case model.current of
                Single _ ->
                    model

                Transition transition ->
                    let
                        { from, to, animation, time } =
                            transition

                        nextTime =
                            time + diff

                        nextAnimation =
                            AnimationState.progress animation nextTime

                        nextTransition =
                            Transition
                                { transition
                                    | time = nextTime
                                    , animation = nextAnimation
                                }
                    in
                        if isDone nextAnimation then
                            { model | current = Single to }
                        else
                            { model | current = nextTransition }

        Push screen ->
            case model.current of
                Single current ->
                    { model
                        | stack = current :: model.stack
                        , current = transition current screen
                    }

                Transition _ ->
                    model

        Pop ->
            let
                current =
                    case model.current of
                        Single screen ->
                            screen

                        Transition { from, to } ->
                            to
            in
                case model.stack of
                    [] ->
                        model

                    last :: rest ->
                        let
                            resetScreen =
                                List.map (\( i, node ) -> ( i, Button.reset node )) last
                        in
                            { model
                                | stack = rest
                                , current = transition current resetScreen
                            }

        Emit _ ->
            model

        ButtonMsg id msg ->
            case model.current of
                Single _ ->
                    case Button.getParentMsg msg of
                        Just msg ->
                            update msg model

                        Nothing ->
                            map
                                (\( i, node ) ->
                                    if id == i then
                                        Button.update msg node
                                    else
                                        node
                                )
                                model

                Transition _ ->
                    model


subscriptions : Model a -> Sub (Msg a)
subscriptions model =
    case model.current of
        Single _ ->
            Sub.none

        Transition _ ->
            AnimationFrame.diffs Frame


viewButton : ( Int, Node a ) -> Html (Msg a)
viewButton ( i, node ) =
    let
        style =
            { defaultColor = hex "32cd32"
            , activeColor = hex "ee9a00"
            , disabledColor = hex "696969"
            }
    in
        div [] [ Html.map (ButtonMsg i) (Button.view style node) ]


viewAnimatedScreen : Float -> List (Html (Msg a)) -> Html (Msg a)
viewAnimatedScreen t screen =
    let
        styles =
            [ transform (scale (3 - (t * 2)))
            , opacity (num (t ^ 2))
            , position absolute
            , top zero
            , Css.width (pct 100)
            ]
    in
        viewScreenWithCss styles screen


viewScreen : List (Html (Msg a)) -> Html (Msg a)
viewScreen screen =
    viewScreenWithCss [] screen


viewScreenWithCss : List Css.Style -> List (Html (Msg a)) -> Html (Msg a)
viewScreenWithCss styles screen =
    let
        viewItem item =
            div [ style (asPairs [ marginBottom (Css.em 1) ]) ]
                [ item ]
    in
        div [ style (asPairs styles) ] (List.map viewItem screen)


viewTransition : TransitionData a -> Html (Msg a)
viewTransition { from, to, time, animation } =
    let
        fromScreen =
            List.map viewButton from

        toScreen =
            List.map viewButton to
    in
        case animation of
            Pending ->
                viewScreen fromScreen

            Done ->
                viewScreen toScreen

            Playing animation ->
                let
                    t =
                        Animation.animate time animation
                in
                    div [ style (asPairs [ position relative ]) ]
                        [ viewAnimatedScreen (1 - t) (List.map viewButton from)
                        , viewAnimatedScreen t (List.map viewButton to)
                        ]


viewView : View a -> Html (Msg a)
viewView view =
    case view of
        Single screen ->
            viewScreen (List.map viewButton screen)

        Transition transition ->
            viewTransition transition


viewBackButton : Html (Msg a)
viewBackButton =
    let
        styles =
            [ fontFamilies [ qt "Press Start 2P" ]
            , textTransform uppercase
            , textShadow4 (px 0) (px 1) (px 2) (rgb 0 0 0)
            , color (hex "FFFFFF")
            , backgroundColor transparent
            , border zero
            , cursor pointer
            ]
    in
        button
            [ style (asPairs styles)
            , onClick Pop
            ]
            [ Html.text "< back" ]


view : Model a -> Html (Msg a)
view { stack, current } =
    let
        back =
            case ( stack, current ) of
                ( _ :: _, Single _ ) ->
                    [ viewBackButton ]

                _ ->
                    []

        main =
            viewView current
    in
        div []
            [ div [ style (asPairs [ minHeight (Css.em 2) ]) ] back
            , main
            ]
