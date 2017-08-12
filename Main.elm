module Main exposing (..)

import Html exposing (Html, button, div, text, input, img)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Mouse exposing (Position)
import Json.Decode as Decode
import Random
import Array exposing (Array)
import Time exposing (Time)
import Debug


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Types


type Msg
    = DragStart Int Position
    | DragAt Position
    | DragEnd Position
    | Shuffle (List ( Int, Int ))
    | Tick Time


type alias Item =
    { id : Int
    , imageTile : Tile
    }


type alias Drag =
    { offset : Position
    , current : Position
    }


type alias Model =
    { activeItem : Maybe ( Item, Tile, Drag )
    , staticItems : List ( Item, Tile )
    , playing : Bool
    , timeout : Int
    }


type alias Tile =
    { row : Int
    , column : Int
    }


type alias Config =
    { tileSize : Int
    , rows : Int
    , columns : Int
    , imageSrc : String
    }


type alias Rectangle =
    { topLeft : Position
    , bottomRight : Position
    }


tileToPosition : Tile -> Position
tileToPosition { row, column } =
    Position
        (column * viewConfig.tileSize)
        (row * viewConfig.tileSize)


positionToTile : Position -> Tile
positionToTile { x, y } =
    Tile
        (round (toFloat y / toFloat viewConfig.tileSize))
        (round (toFloat x / toFloat viewConfig.tileSize))


init : ( Model, Cmd Msg )
init =
    let
        rows =
            List.range 0 (viewConfig.rows - 1)

        columns =
            List.range 0 (viewConfig.columns - 1)

        tiles =
            List.concatMap (\row -> List.map (\column -> Tile row column) columns) rows
    in
        ( { staticItems =
                List.map
                    (\tile ->
                        ( { id = tile.row * viewConfig.columns + tile.column
                          , imageTile = tile
                          }
                        , tile
                        )
                    )
                    tiles
          , activeItem =
                Nothing
          , playing = False
          , timeout = 3
          }
        , Cmd.none
        )


viewConfig : Config
viewConfig =
    { tileSize = 100
    , imageSrc = "/images/01.jpg"
    , rows = 4
    , columns = 4
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        pure model =
            ( model, Cmd.none )
    in
        case msg of
            Tick _ ->
                let
                    length =
                        List.length model.staticItems

                    randomInt =
                        Random.int 0 (length - 1)
                in
                    if model.timeout > 0 then
                        pure { model | timeout = model.timeout - 1 }
                    else
                        ( { model | playing = True }
                        , Random.generate Shuffle
                            (Random.list (length // 3 * 2) (Random.pair randomInt randomInt))
                        )

            Shuffle swaps ->
                pure (updateShuffle swaps model)

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
                        pure (updateDragEnd ( item, tile ) drag model)

                    Nothing ->
                        pure model


updateShuffle : List ( Int, Int ) -> Model -> Model
updateShuffle swaps model =
    let
        staticItems =
            Array.fromList model.staticItems
    in
        { model
            | staticItems =
                List.foldl
                    (\( i, j ) items -> swap i j items)
                    staticItems
                    swaps
                    |> Array.toList
        }


swap : Int -> Int -> Array ( Item, Tile ) -> Array ( Item, Tile )
swap i j arr =
    Maybe.map2
        (\( item1, tile1 ) ( item2, tile2 ) ->
            Array.set j ( item1, tile2 ) (Array.set i ( item2, tile1 ) arr)
        )
        (Array.get i arr)
        (Array.get j arr)
        |> Maybe.withDefault arr


updateDragEnd : ( Item, Tile ) -> Drag -> Model -> Model
updateDragEnd ( activeItem, sourceTile ) drag model =
    let
        targetTile =
            positionToTile (getPosition drag)

        staticItems =
            case List.partition (\( _, tile ) -> tile == targetTile) model.staticItems of
                ( [ ( targetItem, targetTile ) ], staticItems ) ->
                    ( activeItem, targetTile ) :: ( targetItem, sourceTile ) :: staticItems

                _ ->
                    ( activeItem, targetTile ) :: model.staticItems
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
                { x, y } =
                    tileToPosition tile
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
            if not model.playing then
                [ Time.every Time.second Tick ]
            else
                []
    in
        Sub.batch (mouse ++ time)


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


px : Int -> String
px number =
    toString number ++ "px"


viewItem : Bool -> ( Item, Position ) -> List (Html.Attribute Msg) -> Html Msg
viewItem isDragging ( item, { x, y } ) attrs =
    div
        ([ style
            ([ ( "width", px viewConfig.tileSize )
             , ( "height", px viewConfig.tileSize )
             , ( "background", "url(" ++ viewConfig.imageSrc ++ ")" )
             , ( "background-size"
               , px (viewConfig.columns * viewConfig.tileSize)
                    ++ " "
                    ++ px (viewConfig.rows * viewConfig.tileSize)
               )
             , ( "background-position"
               , px (-item.imageTile.column * viewConfig.tileSize)
                    ++ " "
                    ++ px (-item.imageTile.row * viewConfig.tileSize)
               )
             , ( "position", "absolute" )
             , ( "left", px x )
             , ( "top", px y )
             , ( "z-index", "1" )
             ]
                ++ if isDragging then
                    [ ( "box-shadow", "0 0 16px black" ), ( "z-index", "2" ) ]
                   else
                    []
            )
         ]
            ++ attrs
        )
        []


viewDragableItem : Bool -> ( Item, Position ) -> Html Msg
viewDragableItem isDragging itemAndPos =
    let
        ( item, _ ) =
            itemAndPos
    in
        viewItem isDragging
            itemAndPos
            [ on "mousedown" (Decode.map (DragStart item.id) Mouse.position) ]


viewActiveItem : Maybe ( Item, Tile, Drag ) -> List (Html Msg)
viewActiveItem mActiveItem =
    case mActiveItem of
        Nothing ->
            []

        Just ( item, _, drag ) ->
            let
                pos =
                    getPosition drag
            in
                [ viewDragableItem True ( item, pos )
                , viewDropZone (positionToTile pos)
                ]


viewDropZone : Tile -> Html Msg
viewDropZone tile =
    let
        { x, y } =
            tileToPosition tile

        background body =
            div
                [ style
                    [ ( "width", px viewConfig.tileSize )
                    , ( "height", px viewConfig.tileSize )
                    , ( "border", "1px dotted gray" )
                    , ( "left", px x )
                    , ( "top", px y )
                    , ( "position", "absolute" )
                    , ( "z-index", "0" )
                    , ( "background", "transparent" )
                    ]
                ]
                body
    in
        background []


isDone : List ( Item, Tile ) -> Bool
isDone items =
    List.all (\( { imageTile }, tile ) -> imageTile == tile) items


view : Model -> Html Msg
view { activeItem, staticItems, playing, timeout } =
    let
        width =
            viewConfig.tileSize * viewConfig.columns

        height =
            viewConfig.tileSize * viewConfig.rows

        status =
            if isDone staticItems then
                "Done"
            else
                "In Progress"

        header =
            if playing then
                div [] [ text ("Status: " ++ status) ]
            else
                div [] [ text ("Staring in " ++ toString timeout ++ "...") ]
    in
        div []
            [ header
            , div
                [ style
                    [ ( "position", "relative" )
                    , ( "width", px width )
                    , ( "height", px height )
                    ]
                ]
                (List.concat
                    [ viewActiveItem activeItem
                    , List.map
                        (\( item, tile ) ->
                            viewDragableItem False ( item, (tileToPosition tile) )
                        )
                        staticItems
                    ]
                )
            ]
