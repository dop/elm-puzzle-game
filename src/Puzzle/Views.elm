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
import Transition


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
            [ onWithOptions "mousedown"
                { stopPropagation = True, preventDefault = True }
                (Decode.map (DragStart item.id) Mouse.position)
            ]


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


viewFooter : Model -> Html Msg
viewFooter model =
    let
        { playState } =
            model

        barItem body =
            div
                [ style
                    (asPairs
                        [ fontFamilies [ qt "Press Start 2P" ]
                        , fontWeight normal
                        , fontSize (px 12)
                        ]
                    )
                ]
                body

        ( progress, header, time ) =
            case playState of
                Playing n ->
                    let
                        ( done, total ) =
                            getProgress (getAllItems model)
                    in
                        ( barItem [ Html.text (toString done ++ " of " ++ toString total ++ " done.") ]
                        , barItem []
                        , barItem [ Html.text (toString n ++ " points") ]
                        )

                _ ->
                    ( barItem []
                    , barItem []
                    , barItem []
                    )
    in
        div
            [ style
                (asPairs
                    [ displayFlex
                    , justifyContent spaceBetween
                    , margin2 (Css.em 1) zero
                    ]
                )
            ]
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
            ((viewDecorations model) :: puzzle)


viewOverlayWithOpacity : Float -> List (Html Msg) -> Html Msg
viewOverlayWithOpacity opacity =
    div
        [ style
            (asPairs
                [ position absolute
                , backgroundColor (rgba 0 0 0 opacity)
                , left (px 0)
                , right (px 0)
                , top (px 0)
                , bottom (px 0)
                , displayFlex
                , flexDirection column
                , justifyContent center
                , alignItems center
                , zIndex (int 1)
                ]
            )
        ]


viewOverlay : List (Html Msg) -> Html Msg
viewOverlay =
    viewOverlayWithOpacity 0.7


viewButton model =
    Button.view
        { defaultColor = hex "32cd32"
        , activeColor = hex "ee9a00"
        , disabledColor = hex "696969"
        }
        model


viewPlayAgain model =
    viewButton model.playAgainButton
        |> Html.map PlayAgainButtonMsg


viewPlayAnother model =
    viewButton model.playAnotherButton
        |> Html.map PlayRandomButtonMsg


viewDecorations : Model -> Html Msg
viewDecorations model =
    case model.playState of
        Win { transition, score } ->
            if Transition.isDone transition then
                viewWin score 1 model
            else
                Transition.view (div []) transition

        Lose { transition } ->
            if Transition.isDone transition then
                viewLost 1 model
            else
                Transition.view (div []) transition

        Timeout { timeout, transition } ->
            if Transition.isDone transition then
                viewTimeout timeout 1
            else
                Transition.view (div []) transition

        Playing _ ->
            Html.text ""


viewTimeout : Int -> Float -> Html Msg
viewTimeout timeout t =
    viewOverlay
        [ h1
            [ css
                [ fontSize (px 100)
                , transform (scale (1 + 3 * t))
                , opacity (num (1 - t))
                ]
            ]
            [ Html.text (toString timeout) ]
        ]


viewWin : Int -> Float -> Model -> Html Msg
viewWin score t model =
    let
        tBackground =
            bind t ( 0.0, 0.6 )

        tTitle =
            bind t ( 0.0, 0.8 )

        tButton =
            bind t ( 0.4, 1.0 )
    in
        viewOverlayWithOpacity (tBackground * 0.7)
            [ h3
                [ css
                    [ opacity (num tTitle)
                    , transform (scale (1 + (2 * (1 - tTitle))))
                    , marginBottom (Css.em 2)
                    ]
                ]
                [ div
                    [ style (asPairs [ marginBottom (Css.em 0.75) ]) ]
                    [ Html.text "Splendid!" ]
                , div
                    [ style (asPairs [ fontSize (px 32) ]) ]
                    [ Html.text ("+" ++ toString score ++ " points") ]
                ]
            , div
                [ css
                    [ opacity (num tButton)
                    , transform (translateY (px (100 * (1 - tButton))))
                    ]
                ]
                [ viewPlayAnother model ]
            ]


viewLost : Float -> Model -> Html Msg
viewLost t model =
    let
        tBackground =
            bind t ( 0.0, 0.8 )

        tTitle =
            bind t ( 0.1, 0.8 )

        tButton1 =
            bind t ( 0.2, 0.9 )

        tButton2 =
            bind t ( 0.3, 1.0 )
    in
        viewOverlayWithOpacity (tBackground * 0.7)
            [ h3
                [ css
                    [ opacity (num tTitle)
                    , transform (translateY (px (100 * (tTitle - 1))))
                    ]
                ]
                [ Html.text "Time is up! Try again?" ]
            , div
                [ css
                    [ opacity (num tButton1)
                    , transform (translateY (px (100 * (tButton1 - 1))))
                    ]
                , style (asPairs [ marginBottom (Css.em 1) ])
                ]
                [ viewPlayAgain model ]
            , div
                [ css
                    [ opacity (num tButton2)
                    , transform (translateY (px (100 * (tButton2 - 1))))
                    ]
                ]
                [ viewPlayAnother model ]
            ]


bind : Float -> ( Float, Float ) -> Float
bind t ( a, b ) =
    if t <= a then
        0
    else if t >= b then
        1
    else
        (t - a) / (b - a)


css : List Style -> Attribute msg
css styles =
    style (asPairs styles)


view : Model -> Html Msg
view model =
    div []
        [ viewPuzzle model
        , viewFooter model
        ]
