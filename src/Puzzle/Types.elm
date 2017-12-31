module Puzzle.Types exposing (..)

import Mouse exposing (Position)
import Time exposing (Time)
import Button
import Transition


type Msg
    = DragStart Int Position
    | DragAt Position
    | DragEnd Position
    | Shuffle (List Item) (List Tile)
    | Tick Time
    | PlayAgain
    | PlayRandom
    | PickImage Int
    | PlayAgainButtonMsg (Button.Msg Msg)
    | PlayRandomButtonMsg (Button.Msg Msg)
    | TransitionMsg Transition.Msg


type alias Item =
    { id : Int
    , imageTile : Tile
    }


type alias Drag =
    { offset : Position
    , current : Position
    }


type alias Image =
    { url : String
    }


type alias Model =
    { size : Int
    , timeLimit : Int
    , activeItem : Maybe ( Item, Tile, Drag )
    , staticItems : List ( Item, Tile )
    , playState : PlayState
    , image : Image
    , playAnotherButton : Button.Model Msg
    , playAgainButton : Button.Model Msg
    }


type PlayState
    = Playing Int
    | Timeout { timeout : Int, transition : Transition.Model Msg }
    | Win { score : Int, transition : Transition.Model Msg }
    | Lose { transition : Transition.Model Msg }


type alias Tile =
    { row : Int
    , column : Int
    }


type alias Config =
    { tileSize : Int
    , rows : Int
    , columns : Int
    }


type alias Rectangle =
    { topLeft : Position
    , bottomRight : Position
    }
