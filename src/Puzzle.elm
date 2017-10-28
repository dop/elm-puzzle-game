module Puzzle exposing (..)

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
import Html.CssHelpers exposing (withNamespace)
import Elements exposing (..)


type Msg
    = DragStart Int Position
    | DragAt Position
    | DragEnd Position
    | Shuffle (List Item) (List Tile)
    | Tick Time
    | PlayAgain
    | PlayRandom
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
    { size : Int
    , timeLimit : Int
    , activeItem : Maybe ( Item, Tile, Drag )
    , staticItems : List ( Item, Tile )
    , playState : PlayState
    , image : Image
    }


type PlayState
    = Playing Int
    | Timeout Int
    | Win Int
    | Lose


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
          , activeItem =
                Nothing
          , playState =
                Timeout 3
          , image =
                defaultImage
          , size = size
          , timeLimit = timeLimit
          }
        , randomImageCmd
        )


randomImageCmd : Cmd Msg
randomImageCmd =
    Random.generate PickImage (Random.int 0 (Array.length images - 1))


viewConfig : Int -> Config
viewConfig pieces =
    { tileSize = 640 // pieces
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

                    Playing n ->
                        if n > 1 then
                            pure { model | playState = Playing (n - 1) }
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


playAgain : Model -> Model
playAgain model =
    { model
        | playState = Timeout 3
        , staticItems = List.map (\( item, tile ) -> ( item, item.imageTile )) model.staticItems
    }


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


{ id, class, classList } =
    withNamespace ""


viewItem : Config -> Image -> Bool -> ( Item, Position ) -> List (Html.Attribute Msg) -> Html Msg
viewItem config image isDragging ( item, { x, y } ) attrs =
    div
        ([ style
            ([ ( "width", px config.tileSize )
             , ( "height", px config.tileSize )
             , ( "background-image", "url(" ++ image.url ++ ")" )
             , ( "background-size"
               , px (config.columns * config.tileSize)
                    ++ " "
                    ++ px (config.rows * config.tileSize)
               )
             , ( "background-position"
               , px (-item.imageTile.column * config.tileSize)
                    ++ " "
                    ++ px (-item.imageTile.row * config.tileSize)
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


viewNonDragableItem : Config -> Image -> ( Item, Position ) -> Html Msg
viewNonDragableItem config image itemAndPos =
    viewItem config image False itemAndPos []


viewDragableItem : Config -> Image -> Bool -> ( Item, Position ) -> Html Msg
viewDragableItem config image isDragging itemAndPos =
    let
        ( item, _ ) =
            itemAndPos
    in
        viewItem config
            image
            isDragging
            itemAndPos
            [ on "mousedown" (Decode.map (DragStart item.id) Mouse.position) ]


viewActiveItem : Config -> Image -> Maybe ( Item, Tile, Drag ) -> List (Html Msg)
viewActiveItem config image mActiveItem =
    case mActiveItem of
        Nothing ->
            []

        Just ( item, _, drag ) ->
            let
                pos =
                    getPosition drag
            in
                [ viewDragableItem config image True ( item, pos )
                , viewDropZone config (positionToTile config.tileSize pos)
                ]


viewDropZone : Config -> Tile -> Html Msg
viewDropZone config tile =
    let
        { x, y } =
            tileToPosition config.tileSize tile

        background body =
            div
                [ style
                    [ ( "width", px config.tileSize )
                    , ( "height", px config.tileSize )
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
        { activeItem, staticItems, playState, image, size } =
            model

        config =
            viewConfig size

        width =
            config.tileSize * config.columns

        height =
            config.tileSize * config.rows

        barItem body =
            div [ class [ Styles.StatusBarItem ] ] body

        ( progress, header, time ) =
            case playState of
                Playing n ->
                    let
                        ( done, total ) =
                            getProgress (getAllItems model)
                    in
                        ( barItem [ text (toString done ++ " of " ++ toString total ++ " done.") ]
                        , barItem []
                        , barItem [ text (toString n) ]
                        )

                Timeout n ->
                    ( barItem []
                    , barItem [ text ("Starting in " ++ toString n ++ "...") ]
                    , barItem []
                    )

                Win n ->
                    ( barItem []
                    , barItem [ text ("Great job! You scored " ++ toString n ++ " points!") ]
                    , barItem []
                    )

                Lose ->
                    ( barItem []
                    , barItem [ text "Time is up. Try again!" ]
                    , barItem []
                    )

        pickImage value =
            PickImage (Result.withDefault 0 (String.toInt value))

        container body =
            div [ id Styles.Container ] [ body ]

        wrapper body =
            div
                [ id Styles.Wrapper
                , style (Styles.wrapper width)
                ]
                body

        timeBar =
            let
                ( time, limit ) =
                    case model.playState of
                        Playing n ->
                            ( n, model.timeLimit )

                        Win n ->
                            ( n, model.timeLimit )

                        Lose ->
                            ( 0, 1 )

                        Timeout n ->
                            ( n, 3 )

                timeBarWidth =
                    round (toFloat time / toFloat limit * toFloat width)
            in
                div
                    [ style
                        [ ( "width", px width )
                        , ( "height", "2px" )
                        , ( "margin-bottom", "5px" )
                        ]
                    ]
                    [ div
                        [ style
                            [ ( "width", px timeBarWidth )
                            , ( "height", "2px" )
                            , ( "transition", "all .2s" )
                            , ( "background-color", "#ff5522" )
                            ]
                        ]
                        []
                    ]

        progressBar =
            let
                ( done, total ) =
                    getProgress (getAllItems model)

                progressBarWidth =
                    round (toFloat done / toFloat total * toFloat width)
            in
                div
                    [ style
                        [ ( "width", px width )
                        , ( "height", "2px" )
                        , ( "margin-bottom", "5px" )
                        ]
                    ]
                    [ div
                        [ style
                            [ ( "width", px progressBarWidth )
                            , ( "height", "2px" )
                            , ( "transition", "all .2s" )
                            , ( "background-color", "#aaff00" )
                            ]
                        ]
                        []
                    ]

        puzzle =
            List.concat
                [ viewActiveItem config image activeItem
                , List.map
                    (\( item, tile ) ->
                        let
                            itemAndPos =
                                ( item, (tileToPosition config.tileSize tile) )
                        in
                            case playState of
                                Playing _ ->
                                    viewDragableItem config image False itemAndPos

                                _ ->
                                    viewNonDragableItem config image itemAndPos
                    )
                    staticItems
                ]
    in
        div []
            [ Styles.styles
            , container <|
                wrapper <|
                    [ div [ id Styles.StatusBar ]
                        [ progress
                        , header
                        , time
                        ]
                    , progressBar
                    , timeBar
                    , div
                        [ id Styles.PuzzleWrapper
                        , style (Styles.puzzle ( width, height ))
                        ]
                        (let
                            ( opacity, scale ) =
                                case model.playState of
                                    Win _ ->
                                        ( 1, 1 )

                                    _ ->
                                        ( 0, 0 )
                         in
                            div
                                [ style
                                    [ ( "position", "absolute" )
                                    , ( "z-index", "99" )
                                    , ( "top", "50%" )
                                    , ( "left", "50%" )
                                    , ( "font-size", "100px" )
                                    , ( "width", "150px" )
                                    , ( "height", "150px" )
                                    , ( "background", "rgba(255, 255, 255, .9)" )
                                    , ( "display", "flex" )
                                    , ( "align-items", "center" )
                                    , ( "justify-content", "center" )
                                    , ( "border-radius", "50%" )
                                    , ( "transform", "translate(-50%, -50%) scale(" ++ toString scale ++ ")" )
                                    , ( "opacity", toString opacity )
                                    , ( "pointer-events", "none" )
                                    , ( "transition", "all .2s" )
                                    ]
                                ]
                                [ text "👍" ]
                                :: puzzle
                        )
                    , case playState of
                        Win _ ->
                            bigButton "Play Another" PlayRandom

                        Lose ->
                            div []
                                [ bigButton "Play Another" PlayRandom
                                , bigButton "Play Again" PlayAgain
                                ]

                        Timeout _ ->
                            text ""

                        Playing _ ->
                            text ""
                    ]
            ]
