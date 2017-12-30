module Puzzle exposing (..)

import Puzzle.Types exposing (..)
import Puzzle.Utils exposing (..)
import Puzzle.Views exposing (..)
import Mouse exposing (Position)
import Random
import Random.Array
import List.Extra
import Array exposing (Array)
import Time exposing (Time)
import Button


type alias Msg =
    Puzzle.Types.Msg


type alias Model =
    Puzzle.Types.Model


view =
    Puzzle.Views.view


init : Int -> Int -> ( Model, Cmd Msg )
init size timeLimit =
    let
        config =
            viewConfig size

        rows =
            List.range 0 (config.rows - 1)

        columns =
            List.range 0 (config.columns - 1)

        tiles =
            List.concatMap (\row -> List.map (\column -> Tile row column) columns) rows
    in
        ( { staticItems =
                List.map
                    (\tile ->
                        ( { id = tile.row * config.columns + tile.column
                          , imageTile = tile
                          }
                        , tile
                        )
                    )
                    tiles
          , activeItem = Nothing
          , playState = Timeout 3
          , image = defaultImage
          , size = size
          , timeLimit = timeLimit
          , playAgainButton = Button.init PlayAgain "Try Again"
          , playAnotherButton = Button.init PlayRandom "Play Another"
          }
        , randomImageCmd
        )


randomImageCmd : Cmd Msg
randomImageCmd =
    Random.generate PickImage (Random.int 0 (Array.length images - 1))


defaultImage : Image
defaultImage =
    { url = "/images/01.jpg" }


images : Array Image
images =
    Array.fromList
        [ defaultImage
        , { url = "/images/02.jpg" }
        , { url = "/images/03.jpg" }
        , { url = "/images/04.jpg" }
        , { url = "/images/05.jpg" }
        , { url = "/images/06.jpg" }
        , { url = "/images/07.jpg" }
        , { url = "/images/08.jpg" }
        , { url = "/images/09.jpg" }
        , { url = "/images/10.jpg" }
        , { url = "/images/11.jpg" }
        , { url = "/images/12.jpg" }
        ]


shuffleCmd : List ( Item, Tile ) -> Cmd Msg
shuffleCmd pairs =
    let
        items =
            List.map Tuple.first pairs

        tiles =
            List.map Tuple.second pairs
    in
        Random.generate (Shuffle items)
            ((Random.Array.shuffle <| Array.fromList tiles)
                |> Random.map Array.toList
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        pure model =
            ( model, Cmd.none )
    in
        case msg of
            PickImage n ->
                case Array.get n images of
                    Just image ->
                        pure { model | image = image }

                    Nothing ->
                        pure { model | image = defaultImage }

            Tick _ ->
                case model.playState of
                    Timeout timeout ->
                        if timeout > 1 then
                            pure { model | playState = Timeout (timeout - 1) }
                        else
                            ( { model | playState = Playing model.timeLimit }
                            , shuffleCmd model.staticItems
                            )

                    Playing points ->
                        if points > 1 then
                            pure { model | playState = Playing (points - 1) }
                        else
                            pure { model | playState = Lose }

                    _ ->
                        pure model

            Shuffle items tiles ->
                pure { model | staticItems = List.Extra.zip items tiles }

            DragStart i xy ->
                pure (updateDragStart model xy i)

            DragAt xy ->
                case model.activeItem of
                    Just ( item, tile, drag ) ->
                        pure
                            { model
                                | activeItem =
                                    Just ( item, tile, Drag drag.offset xy )
                            }

                    Nothing ->
                        pure model

            DragEnd _ ->
                case model.activeItem of
                    Just ( item, tile, drag ) ->
                        let
                            newModel =
                                updateDragEnd ( item, tile ) drag model

                            playState =
                                if isDone newModel.staticItems then
                                    toWin newModel.playState
                                else
                                    newModel.playState
                        in
                            pure { newModel | playState = playState }

                    Nothing ->
                        pure model

            PlayAgain ->
                ( playAgain model
                , Cmd.none
                )

            PlayRandom ->
                ( playAgain model
                , randomImageCmd
                )

            PlayAgainButtonMsg msg ->
                case Button.getParentMsg msg of
                    Just msg ->
                        update msg model

                    Nothing ->
                        pure { model | playAgainButton = Button.update msg model.playAgainButton }

            PlayRandomButtonMsg msg ->
                case Button.getParentMsg msg of
                    Just msg ->
                        update msg model

                    Nothing ->
                        pure { model | playAnotherButton = Button.update msg model.playAnotherButton }


playAgain : Model -> Model
playAgain model =
    { model
        | playState = Timeout 3
        , staticItems = List.map (\( item, tile ) -> ( item, item.imageTile )) model.staticItems
    }


updateDragEnd : ( Item, Tile ) -> Drag -> Model -> Model
updateDragEnd ( activeItem, sourceTile ) drag model =
    let
        config =
            viewConfig model.size

        targetTile =
            positionToTile config.tileSize (getPosition drag)

        staticItems =
            if tileInBounds ( config.rows, config.columns ) targetTile then
                case List.partition (\( _, tile ) -> tile == targetTile) model.staticItems of
                    ( [ ( targetItem, targetTile ) ], staticItems ) ->
                        ( activeItem, targetTile ) :: ( targetItem, sourceTile ) :: staticItems

                    _ ->
                        ( activeItem, targetTile ) :: model.staticItems
            else
                ( activeItem, sourceTile ) :: model.staticItems
    in
        { model
            | activeItem = Nothing
            , staticItems = staticItems
        }


updateDragStart : Model -> Position -> Int -> Model
updateDragStart model click i =
    case List.partition (\( item, _ ) -> item.id == i) model.staticItems of
        ( [ ( activeItem, tile ) ], staticItems ) ->
            let
                config =
                    viewConfig model.size

                { x, y } =
                    tileToPosition config.tileSize tile
            in
                { model
                    | activeItem =
                        Just
                            ( activeItem
                            , tile
                            , Drag { x = click.x - x, y = click.y - y } click
                            )
                    , staticItems = staticItems
                }

        _ ->
            model


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        mouse =
            case model.activeItem of
                Nothing ->
                    []

                Just _ ->
                    [ Mouse.moves DragAt, Mouse.ups DragEnd ]

        time =
            case model.playState of
                Timeout _ ->
                    [ Time.every Time.second Tick ]

                Playing _ ->
                    [ Time.every Time.second Tick ]

                _ ->
                    []
    in
        Sub.batch (mouse ++ time)
