module Puzzle.Views exposing (..)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Mouse
import Puzzle.Types exposing (..)
import Puzzle.Utils exposing (..)
import Json.Decode as Decode
import Button


viewConfig : Int -> Config
viewConfig pieces =
    { tileSize = 640 // pieces
    , rows = pieces
    , columns = pieces
    }


getProgress : List ( Item, Tile ) -> ( Int, Int )
getProgress items =
    let
        total =
            List.length items

        done =
            List.length (List.filter itemIsDone items)
    in
        ( done, total )


viewItem : Config -> Image -> Bool -> ( Item, Mouse.Position ) -> List (Html.Attribute Msg) -> Html Msg
viewItem config image isDragging ( item, { x, y } ) attrs =
    let
        boxShadowRule =
            if isDragging then
                boxShadow4 zero zero (px 16) (hex "000000")
            else
                boxShadow none

        zidx =
            if isDragging then
                2
            else
                0
    in
        div
            ([ style
                (asPairs
                    [ Css.width (px (toFloat config.tileSize))
                    , Css.height (px (toFloat config.tileSize))
                    , backgroundImage (url image.url)
                    , backgroundSize2
                        (px (toFloat (config.columns * config.tileSize)))
                        (px (toFloat (config.rows * config.tileSize)))
                    , backgroundPosition2
                        (px (toFloat (-item.imageTile.column * config.tileSize)))
                        (px (toFloat (-item.imageTile.row * config.tileSize)))
                    , position absolute
                    , left (px (toFloat x))
                    , top (px (toFloat y))
                    , zIndex (int zidx)
                    , boxShadowRule
                    ]
                )
             ]
                ++ attrs
            )
            []


viewNonDragableItem : Config -> Image -> ( Item, Mouse.Position ) -> Html Msg
viewNonDragableItem config image itemAndPos =
    viewItem config image False itemAndPos []


viewDragableItem : Config -> Image -> Bool -> ( Item, Mouse.Position ) -> Html Msg
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
    in
        div
            [ style
                (asPairs
                    [ Css.width (px (toFloat config.tileSize))
                    , Css.height (px (toFloat config.tileSize))
                    , outline3 (px 3) solid (hex "ff0000")
                    , left (px (toFloat x))
                    , top (px (toFloat y))
                    , position absolute
                    , zIndex (int 1)
                    , backgroundColor transparent
                    ]
                )
            ]
            []


viewHeader : Model -> Html Msg
viewHeader model =
    let
        { playState } =
            model

        barItem body =
            div [] body

        ( progress, header, time ) =
            case playState of
                Playing n ->
                    let
                        ( done, total ) =
                            getProgress (getAllItems model)
                    in
                        ( barItem [ Html.text (toString done ++ " of " ++ toString total ++ " done.") ]
                        , barItem []
                        , barItem [ Html.text (toString n) ]
                        )

                Timeout n ->
                    ( barItem []
                    , barItem [ Html.text ("Starting in " ++ toString n ++ "...") ]
                    , barItem []
                    )

                Win n ->
                    ( barItem []
                    , barItem [ Html.text ("Great job! You scored " ++ toString n ++ " points!") ]
                    , barItem []
                    )

                Lose ->
                    ( barItem []
                    , barItem [ Html.text "Time is up. Try again!" ]
                    , barItem []
                    )
    in
        div []
            [ progress
            , header
            , time
            ]


viewPuzzle : Model -> Html Msg
viewPuzzle model =
    let
        { activeItem, staticItems, playState, image, size } =
            model

        config =
            viewConfig size

        width =
            config.tileSize * config.columns

        height =
            config.tileSize * config.rows

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
        div
            [ style
                (asPairs
                    [ position relative
                    , margin2 zero auto
                    , backgroundColor (hex "000000")
                    , Css.width (toFloat width |> px)
                    , Css.height (toFloat height |> px)
                    ]
                )
            ]
            (let
                ( opacity_, scale_ ) =
                    case model.playState of
                        Win _ ->
                            ( 1, 1 )

                        _ ->
                            ( 0, 0 )
             in
                div
                    [ style
                        (asPairs
                            [ position absolute
                            , zIndex (int 99)
                            , top (pct 50)
                            , left (pct 50)
                            , fontSize (px 100)
                            , Css.width (px 150)
                            , Css.height (px 150)
                            , backgroundColor (rgba 255 255 255 0.9)
                            , displayFlex
                            , alignItems center
                            , justifyContent center
                            , borderRadius (pct 50)
                            , transforms [ translate2 (pct (-50)) (pct (-50)), scale scale_ ]
                            , opacity (num opacity_)
                            , Css.property "pointer-events" "none"
                            , Css.property "transition" "all .2s"
                            ]
                        )
                    ]
                    [ Html.text "👍" ]
                    :: puzzle
            )


view : Model -> Html Msg
view model =
    let
        viewButton model =
            Button.view
                { defaultColor = hex "ff0000"
                , activeColor = hex "00ff00"
                , disabledColor = hex "696969"
                }
                model

        viewPlayAgain () =
            viewButton model.playAgainButton
                |> Html.map PlayAgainButtonMsg

        viewPlayAnother () =
            viewButton model.playAnotherButton
                |> Html.map PlayRandomButtonMsg
    in
        div []
            [ viewHeader model
            , viewPuzzle model
            , case model.playState of
                Win _ ->
                    viewPlayAgain ()

                Lose ->
                    div []
                        [ viewPlayAnother ()
                        , viewPlayAgain ()
                        ]

                Timeout _ ->
                    Html.text ""

                Playing _ ->
                    Html.text ""
            ]
