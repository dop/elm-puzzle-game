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
import Task


type alias TransitionData a =
    AnimationData { from : Screen a, to : Screen a }


type alias FadeInData a =
    AnimationData { to : Screen a }


type alias FadeOutData a =
    AnimationData { from : Screen a }


type View a
    = Single (Screen a)
    | Transition (TransitionData a)
    | FadeIn (FadeInData a)
    | FadeOut (FadeOutData a) (Msg a)
    | None


type alias Model a =
    { stack : List (Screen a)
    , current : View a
    }


type Msg a
    = Push (Screen a)
    | Pop
    | Emit a
    | Exit a
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
    , current =
        FadeIn
            { to = enum tree
            , time = 0
            , animation =
                Playing
                    (Animation.animation 0
                        |> Animation.ease Ease.inOutQuad
                        |> Animation.duration (0.4 * Time.second)
                    )
            }
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
    Button.init (Exit msg) title


map : (( Int, Node a ) -> Node a) -> Model a -> Model a
map f model =
    let
        current =
            case model.current of
                Single screen ->
                    Single (enum (List.map f screen))

                Transition transition ->
                    Transition { transition | to = enum (List.map f transition.to) }

                FadeIn fade ->
                    FadeIn { fade | to = enum (List.map f fade.to) }

                FadeOut fade msg ->
                    FadeOut fade msg

                None ->
                    None
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


pure : Model a -> ( Model a, Cmd (Msg a) )
pure model =
    ( model, Cmd.none )


send : Msg a -> Cmd (Msg a)
send msg =
    Task.succeed msg |> Task.perform identity


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg model =
    case msg of
        Frame diff ->
            case model.current of
                Single _ ->
                    pure model

                None ->
                    pure model

                FadeIn fade ->
                    let
                        { to, animation, time } =
                            fade

                        nextTime =
                            time + diff

                        nextAnimation =
                            AnimationState.progress animation nextTime

                        nextFade =
                            FadeIn
                                { fade
                                    | animation = nextAnimation
                                    , time = nextTime
                                }
                    in
                        if isDone nextAnimation then
                            pure { model | current = Single to }
                        else
                            pure { model | current = nextFade }

                FadeOut fade msg ->
                    let
                        { from, animation, time } =
                            fade

                        nextTime =
                            time + diff

                        nextAnimation =
                            AnimationState.progress animation nextTime

                        nextFade =
                            FadeOut
                                { fade
                                    | animation = nextAnimation
                                    , time = nextTime
                                }
                                msg
                    in
                        if isDone nextAnimation then
                            ( { model | current = None }, send msg )
                        else
                            pure { model | current = nextFade }

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
                            pure { model | current = Single to }
                        else
                            pure { model | current = nextTransition }

        Push screen ->
            pure
                (case model.current of
                    Single current ->
                        { model
                            | stack = current :: model.stack
                            , current = transition current screen
                        }

                    FadeIn _ ->
                        model

                    FadeOut _ _ ->
                        model

                    Transition _ ->
                        model

                    None ->
                        model
                )

        Pop ->
            case model.stack of
                [] ->
                    pure model

                last :: rest ->
                    let
                        resetScreen =
                            List.map (\( i, node ) -> ( i, Button.reset node )) last

                        transitionTo from =
                            transition from resetScreen

                        current =
                            case model.current of
                                Single screen ->
                                    transitionTo screen

                                Transition { from, to } ->
                                    transitionTo to

                                FadeIn { to } ->
                                    transitionTo to

                                FadeOut _ _ ->
                                    model.current

                                None ->
                                    model.current
                    in
                        pure
                            { model
                                | stack = rest
                                , current = current
                            }

        Exit msg ->
            pure
                (case model.current of
                    Single screen ->
                        { model
                            | current =
                                FadeOut
                                    { from = screen
                                    , time = 0
                                    , animation =
                                        Playing
                                            (Animation.animation 0
                                                |> Animation.ease Ease.inOutQuad
                                                |> Animation.duration (0.4 * Time.second)
                                            )
                                    }
                                    (Emit msg)
                        }

                    _ ->
                        model
                )

        Emit _ ->
            pure model

        ButtonMsg id msg ->
            case model.current of
                Single _ ->
                    case Button.getParentMsg msg of
                        Just msg ->
                            update msg model

                        Nothing ->
                            pure
                                (map
                                    (\( i, node ) ->
                                        if id == i then
                                            Button.update msg node
                                        else
                                            node
                                    )
                                    model
                                )

                Transition _ ->
                    pure model

                FadeIn _ ->
                    pure model

                FadeOut _ _ ->
                    pure model

                None ->
                    pure model


subscriptions : Model a -> Sub (Msg a)
subscriptions model =
    let
        frames =
            AnimationFrame.diffs Frame
    in
        case model.current of
            Single _ ->
                Sub.none

            Transition _ ->
                frames

            FadeIn _ ->
                frames

            FadeOut _ _ ->
                frames

            None ->
                Sub.none


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
                        [ viewAnimatedScreen (1 - t) fromScreen
                        , viewAnimatedScreen t toScreen
                        ]


viewFadeIn : FadeInData a -> Html (Msg a)
viewFadeIn { to, time, animation } =
    let
        toScreen =
            List.map viewButton to
    in
        case animation of
            Pending ->
                div [] []

            Done ->
                viewScreen toScreen

            Playing animation ->
                let
                    t =
                        Animation.animate time animation
                in
                    div [ style (asPairs [ position relative ]) ]
                        [ viewAnimatedScreen t toScreen
                        ]


viewFadeOut : FadeOutData a -> Html (Msg a)
viewFadeOut { from, time, animation } =
    let
        fromScreen =
            List.map viewButton from
    in
        case animation of
            Pending ->
                div [] []

            Done ->
                viewScreen fromScreen

            Playing animation ->
                let
                    t =
                        Animation.animate time animation
                in
                    div [ style (asPairs [ position relative ]) ]
                        [ viewAnimatedScreen (1 - t) fromScreen
                        ]


viewView : View a -> Html (Msg a)
viewView view =
    case view of
        Single screen ->
            viewScreen (List.map viewButton screen)

        Transition transition ->
            viewTransition transition

        FadeIn fade ->
            viewFadeIn fade

        FadeOut fade _ ->
            viewFadeOut fade

        None ->
            div [] []


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
