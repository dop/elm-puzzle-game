module Puzzle.Utils exposing (..)

import Puzzle.Types exposing (..)
import Mouse exposing (Position)


tileToPosition : Int -> Tile -> Position
tileToPosition tileSize { row, column } =
    Position
        (column * tileSize)
        (row * tileSize)


positionToTile : Int -> Position -> Tile
positionToTile tileSize { x, y } =
    Tile
        (round (toFloat y / toFloat tileSize))
        (round (toFloat x / toFloat tileSize))


itemIsDone : ( Item, Tile ) -> Bool
itemIsDone ( { imageTile }, tile ) =
    imageTile == tile


isDone : List ( Item, Tile ) -> Bool
isDone items =
    List.all itemIsDone items


tileInBounds : ( Int, Int ) -> Tile -> Bool
tileInBounds ( rows, columns ) { row, column } =
    row >= 0 && row < rows && column >= 0 && column < columns


getAllItems : Model -> List ( Item, Tile )
getAllItems { staticItems, activeItem } =
    case activeItem of
        Just ( item, tile, drag ) ->
            ( item, tile ) :: staticItems

        Nothing ->
            staticItems


formatSeconds : Int -> String
formatSeconds n =
    let
        seconds =
            n % 60

        minutes =
            n // 60

        pad string =
            if String.length string == 1 then
                "0" ++ string
            else
                string
    in
        toString minutes ++ ":" ++ pad (toString seconds)


isInside : Position -> Rectangle -> Bool
isInside { x, y } { topLeft, bottomRight } =
    if
        (x
            >= topLeft.x
            && x
            <= bottomRight.x
            && y
            >= topLeft.y
            && y
            <= bottomRight.y
        )
    then
        True
    else
        False


isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Just _ ->
            False

        Nothing ->
            True


getPosition : Drag -> Position
getPosition { offset, current } =
    Position
        (current.x - offset.x)
        (current.y - offset.y)
