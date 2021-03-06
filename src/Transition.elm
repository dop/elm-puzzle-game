module Transition exposing (..)

import AnimationState exposing (..)
import Animation exposing (Animation)
import AnimationFrame
import Ease
import Time exposing (Time)
import Html exposing (..)


type Model a
    = Static (Html a)
    | Transition (TransitionData a)


type alias TransitionData a =
    AnimationData
        { from : Float -> Html a
        , to : Float -> Html a
        }


type alias Config =
    { duration : Float
    , ease : Ease.Easing
    , start : Float
    }


type Msg
    = Frame Time



-- CONSTRUCTORS


defaults : Config
defaults =
    { duration = 0.4, start = 0, ease = Ease.inOutQuad }


fadeIn : (Float -> Html a) -> Model a
fadeIn to =
    transition (\t -> div [] []) to


fadeOut : (Float -> Html a) -> Model a
fadeOut from =
    transition from (\t -> div [] [])


fadeInWith : Config -> (Float -> Html a) -> Model a
fadeInWith config to =
    transitionWith config (\t -> div [] []) to


fadeOutWith : Config -> (Float -> Html a) -> Model a
fadeOutWith config from =
    transitionWith config from (\t -> div [] [])


transition : (Float -> Html a) -> (Float -> Html a) -> Model a
transition =
    transitionWith defaults


transitionWith : Config -> (Float -> Html a) -> (Float -> Html a) -> Model a
transitionWith { duration, ease, start } from to =
    Transition
        { from = from
        , to = to
        , time = 0
        , animation =
            Playing
                (Animation.animation start
                    |> Animation.ease ease
                    |> Animation.duration (duration * Time.second)
                )
        }


static : Html a -> Model a
static view =
    Static view


isDone : Model a -> Bool
isDone model =
    case model of
        Static _ ->
            True

        Transition _ ->
            False



-- UPDATE


pure : Model a -> ( Model a, Cmd Msg )
pure model =
    ( model, Cmd.none )


update : Msg -> Model a -> Model a
update msg model =
    case msg of
        Frame diff ->
            case model of
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
                        if AnimationState.isDone nextAnimation then
                            Static (to 1)
                        else
                            nextTransition

                Static html ->
                    model


subscriptions : Model a -> Sub Msg
subscriptions model =
    case model of
        Static _ ->
            Sub.none

        Transition _ ->
            AnimationFrame.diffs Frame



-- VIEWS


view : (List (Html a) -> Html a) -> Model a -> Html a
view wrap model =
    case model of
        Static html ->
            wrap [ html ]

        Transition transition ->
            wrap (viewTransition transition)


viewTransition : TransitionData a -> List (Html a)
viewTransition { from, to, time, animation } =
    case animation of
        Pending ->
            [ from 1 ]

        Done ->
            [ to 1 ]

        Playing animation ->
            let
                t =
                    Animation.animate time animation
            in
                [ from (1 - t)
                , to t
                ]
