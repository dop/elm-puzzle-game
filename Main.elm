module Main exposing (..)

import Html exposing (Html, button, div, text, input, img, select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import Mouse exposing (Position)
import Json.Decode as Decode
import Random
import Random.Array
import List.Extra
import Array exposing (Array)
import Time exposing (Time)
import Styles


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = DragStart Int Position
    | DragAt Position
    | DragEnd Position
    | Shuffle (List Item) (List Tile)
    | Tick Time
    | PlayAgain
    | PickImage Int


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
    , playState : PlayState
    , image : Image
    }


type PlayState
    = Playing Int
    | Timeout Int
    | Win Int


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
          , playState =
                Timeout 3
          , image =
                defaultImage
          }
        , randomImageCmd
        )


randomImageCmd : Cmd Msg
randomImageCmd =
    Random.generate PickImage (Random.int 0 (Array.length images - 1))


viewConfig : Config
viewConfig =
    let
        size =
            640

        pieces =
            4
    in
        { tileSize = size // pieces
        , rows = pieces
        , columns = pieces
        }


type alias Image =
    { url : String
    }


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
                            ( { model | playState = Playing 0 }
                            , shuffleCmd model.staticItems
                            )

                    Playing n ->
                        pure { model | playState = Playing (n + 1) }

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
                ( { model | playState = Timeout 3 }
                , randomImageCmd
                )


toWin : PlayState -> PlayState
toWin state =
    case state of
        Playing n ->
            Win n

        _ ->
            Win 0


itemIsDone : ( Item, Tile ) -> Bool
itemIsDone ( { imageTile }, tile ) =
    imageTile == tile


isDone : List ( Item, Tile ) -> Bool
isDone items =
    List.all itemIsDone items


getProgress : List ( Item, Tile ) -> ( Int, Int )
getProgress items =
    let
        total =
            List.length items

        done =
            List.length (List.filter itemIsDone items)
    in
        ( done, total )


tileInBounds : ( Int, Int ) -> Tile -> Bool
tileInBounds ( rows, columns ) { row, column } =
    row >= 0 && row < rows && column >= 0 && column < columns


updateDragEnd : ( Item, Tile ) -> Drag -> Model -> Model
updateDragEnd ( activeItem, sourceTile ) drag model =
    let
        targetTile =
            positionToTile (getPosition drag)

        staticItems =
            if tileInBounds ( viewConfig.rows, viewConfig.columns ) targetTile then
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
            case model.playState of
                Timeout _ ->
                    [ Time.every Time.second Tick ]

                Playing _ ->
                    [ Time.every Time.second Tick ]

                _ ->
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


viewItem : Image -> Bool -> ( Item, Position ) -> List (Html.Attribute Msg) -> Html Msg
viewItem image isDragging ( item, { x, y } ) attrs =
    div
        ([ style
            ([ ( "width", px viewConfig.tileSize )
             , ( "height", px viewConfig.tileSize )
             , ( "background-image", "url(" ++ image.url ++ ")" )
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
             , ( "z-index"
               , if isDragging then
                    "2"
                 else
                    "0"
               )
             , ( "box-shadow"
               , if isDragging then
                    "0 0 16px black"
                 else
                    "none"
               )
             ]
            )
         ]
            ++ attrs
        )
        []


viewNonDragableItem : Image -> ( Item, Position ) -> Html Msg
viewNonDragableItem image itemAndPos =
    viewItem image False itemAndPos []


viewDragableItem : Image -> Bool -> ( Item, Position ) -> Html Msg
viewDragableItem image isDragging itemAndPos =
    let
        ( item, _ ) =
            itemAndPos
    in
        viewItem image
            isDragging
            itemAndPos
            [ on "mousedown" (Decode.map (DragStart item.id) Mouse.position) ]


viewActiveItem : Image -> Maybe ( Item, Tile, Drag ) -> List (Html Msg)
viewActiveItem image mActiveItem =
    case mActiveItem of
        Nothing ->
            []

        Just ( item, _, drag ) ->
            let
                pos =
                    getPosition drag
            in
                [ viewDragableItem image True ( item, pos )
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
                    , ( "outline", "3px solid red" )
                    , ( "left", px x )
                    , ( "top", px y )
                    , ( "position", "absolute" )
                    , ( "z-index", "1" )
                    , ( "background", "transparent" )
                    ]
                ]
                body
    in
        background []


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


getAllItems : Model -> List ( Item, Tile )
getAllItems { staticItems, activeItem } =
    case activeItem of
        Just ( item, tile, drag ) ->
            ( item, tile ) :: staticItems

        Nothing ->
            staticItems


view : Model -> Html Msg
view model =
    let
        { activeItem, staticItems, playState, image } =
            model

        width =
            viewConfig.tileSize * viewConfig.columns

        height =
            viewConfig.tileSize * viewConfig.rows

        barItem body =
            div [ Styles.statusBarItem ] body

        ( progress, header, time ) =
            case playState of
                Playing n ->
                    let
                        ( done, total ) =
                            getProgress (getAllItems model)
                    in
                        ( barItem [ text (toString done ++ " of " ++ toString total ++ " done.") ]
                        , barItem []
                        , barItem [ text (formatSeconds n) ]
                        )

                Timeout n ->
                    ( barItem []
                    , barItem [ text ("Starting in " ++ toString n ++ "...") ]
                    , barItem []
                    )

                Win n ->
                    ( barItem []
                    , barItem [ text ("Done! You took " ++ formatSeconds n) ]
                    , barItem []
                    )

        pickImage value =
            PickImage (Result.withDefault 0 (String.toInt value))

        container body =
            div [ Styles.container ] [ body ]

        wrapper body =
            div [ Styles.wrapper (toFloat width) ] body
    in
        container <|
            wrapper <|
                [ div [ Styles.statusBar ]
                    [ progress
                    , header
                    , time
                    ]
                , div
                    [ Styles.puzzleWrapper ( toFloat width, toFloat height )
                    ]
                    (List.concat
                        [ viewActiveItem image activeItem
                        , List.map
                            (\( item, tile ) ->
                                let
                                    itemAndPos =
                                        ( item, (tileToPosition tile) )
                                in
                                    case playState of
                                        Playing _ ->
                                            viewDragableItem image False itemAndPos

                                        _ ->
                                            viewNonDragableItem image itemAndPos
                            )
                            staticItems
                        ]
                    )
                , case playState of
                    Win _ ->
                        button
                            [ Styles.playAgainButton
                            , onClick PlayAgain
                            ]
                            [ text "Play Again" ]

                    _ ->
                        text ""
                ]
