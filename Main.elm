module Main exposing (..)

import Html exposing (Html, button, div, text, input, img)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Mouse exposing (Position)
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Item =
    { id : Int
    }


type alias Drag =
    { offset : Position
    , current : Position
    }


type alias Model =
    { activeItem : Maybe ( Item, Drag )
    , staticItems : List ( Item, Tile )
    }


type alias Tile =
    { row : Int
    , column : Int
    }


type alias Config =
    { tileSize : Int
    , imageSrc : String
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
    ( { staticItems =
            [ ( { id = 0 }, Tile 0 0 )
            , ( { id = 1 }, Tile 1 0 )
            , ( { id = 2 }, Tile 0 1 )
            , ( { id = 3 }, Tile 1 1 )
            , ( { id = 4 }, Tile 1 2 )
            , ( { id = 5 }, Tile 2 1 )
            , ( { id = 6 }, Tile 2 2 )
            , ( { id = 7 }, Tile 1 3 )
            , ( { id = 8 }, Tile 0 3 )
            , ( { id = 9 }, Tile 2 3 )
            , ( { id = 10 }, Tile 2 0 )
            , ( { id = 11 }, Tile 0 2 )
            , ( { id = 12 }, Tile 3 0 )
            , ( { id = 13 }, Tile 3 1 )
            , ( { id = 14 }, Tile 3 2 )
            , ( { id = 15 }, Tile 3 3 )
            ]
      , activeItem = Nothing
      }
    , Cmd.none
    )


viewConfig : Config
viewConfig =
    { tileSize = 100
    , imageSrc = "/images/01.jpg"
    }


type Msg
    = DragStart Int Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        pure model =
            ( model, Cmd.none )
    in
        case msg of
            DragStart i xy ->
                pure (updateDragStart model xy i)

            DragAt xy ->
                case model.activeItem of
                    Just ( item, drag ) ->
                        pure
                            { model
                                | activeItem =
                                    Just ( item, Drag drag.offset xy )
                            }

                    Nothing ->
                        pure model

            DragEnd _ ->
                case model.activeItem of
                    Just ( item, drag ) ->
                        pure (updateDragEnd item drag model)

                    Nothing ->
                        pure model


updateDragEnd : Item -> Drag -> Model -> Model
updateDragEnd item drag model =
    let
        staticItems =
            ( item, positionToTile (getPosition drag) ) :: model.staticItems

        snapItem id dropTile ( item, tile ) =
            if item.id == id then
                ( item, dropTile )
            else
                ( item, tile )

        snapItems { item, tile } staticItems =
            case item of
                Nothing ->
                    staticItems

                Just { id } ->
                    List.map (snapItem id tile) staticItems
    in
        { model
            | activeItem = Nothing
            , staticItems =
                List.foldl snapItems staticItems []
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
                            , Drag { x = click.x - x, y = click.y - y } click
                            )
                    , staticItems = staticItems
                }

        _ ->
            model


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.activeItem of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]


type alias Rectangle =
    { topLeft : Position
    , bottomRight : Position
    }


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
             , ( "background-size", px (4 * viewConfig.tileSize) )
             , ( "background-position"
               , px (item.id // 4 * viewConfig.tileSize)
                    ++ " "
                    ++ px (item.id % 4 * viewConfig.tileSize)
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
        [ text (toString item.id) ]


viewDragableItem : Bool -> ( Item, Position ) -> Html Msg
viewDragableItem isDragging itemAndPos =
    let
        ( item, _ ) =
            itemAndPos
    in
        viewItem isDragging
            itemAndPos
            [ on "mousedown" (Decode.map (DragStart item.id) Mouse.position) ]


viewActiveItem : Maybe ( Item, Drag ) -> List (Html Msg)
viewActiveItem mActiveItem =
    case mActiveItem of
        Nothing ->
            []

        Just ( item, drag ) ->
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


view : Model -> Html Msg
view { activeItem, staticItems } =
    div []
        [ div []
            (List.concat
                [ viewActiveItem activeItem
                , List.map
                    (\( item, tile ) ->
                        viewDragableItem False ( item, (tileToPosition tile) )
                    )
                    staticItems
                ]
            )

        -- , img [ src "/images/01.jpg" ] []
        ]
