module AnimationState exposing (..)

import Ease
import Animation exposing (Animation)
import AnimationFrame
import Time exposing (Time)


type AnimationState
    = Pending
    | Playing Animation
    | Done


type alias AnimationData a =
    { a | time : Time, animation : AnimationState }


progress : AnimationState -> Time -> AnimationState
progress state t =
    case state of
        Playing animation ->
            if Animation.isDone t animation then
                Done
            else
                Playing animation

        _ ->
            state


isDone : AnimationState -> Bool
isDone state =
    case state of
        Done ->
            True

        _ ->
            False


isPending : AnimationState -> Bool
isPending state =
    case state of
        Pending ->
            True

        _ ->
            False


isPlaying : AnimationState -> Bool
isPlaying state =
    case state of
        Playing _ ->
            True

        _ ->
            False
