module Menu
    exposing
        ( init
        , update
        , view
        , subscriptions
        , Model
        , Msg
        , node
        , leaf
        , getParentMsg
        )

import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Button
import Task
import Transition exposing (TransitionData)


type alias Model a =
    { stack : List (Screen a)
    , current : View a
    }


type View a
    = Single (Screen a)
    | Transition
        { screens : { from : Screen a, to : Screen a }
        , transition : Transition.Model (Msg a)
        }
    | None


type Msg a
    = Push (Screen a)
    | Pop
    | Emit a
    | Exit a
    | ButtonMsg Int (Button.Msg (Msg a))
    | TransitionMsg Transition.Msg


type Screen a
    = Menu (List ( Int, Node a ))
    | Embed (Html (Msg a))


screenMap : (( Int, Node a ) -> ( Int, Node a )) -> Screen a -> Screen a
screenMap f screen =
    case screen of
        Embed html ->
            Embed html

        Menu nodes ->
            Menu (List.map f nodes)


type alias Node a =
    Button.Model (Msg a)


getParentMsg : Msg a -> Maybe a
getParentMsg msg =
    case msg of
        Emit msg ->
            Just msg

        ButtonMsg _ msg ->
            Button.getParentMsg msg
                |> Maybe.andThen getParentMsg

        _ ->
            Nothing


init : List (Node a) -> Model a
init tree =
    { stack = []
    , current = fadeIn (Menu (enum tree))
    }


emptyScreen : Screen a
emptyScreen =
    Menu []


fadeIn : Screen a -> View a
fadeIn screen =
    let
        from _ =
            div [] []

        to t =
            viewAnimatedScreen t screen
    in
        Transition
            { screens = { to = screen, from = emptyScreen }
            , transition = Transition.transition from to
            }


fadeOut : Screen a -> Msg a -> View a
fadeOut screen msg =
    let
        to _ =
            div [] []

        from t =
            viewAnimatedScreen t screen
    in
        Transition
            { screens = { to = emptyScreen, from = screen }
            , transition = Transition.transition from to
            }


enum : List a -> List ( Int, a )
enum list =
    let
        iter i xs =
            case xs of
                [] ->
                    []

                x :: xs ->
                    ( i, x ) :: iter (i + 1) xs
    in
        iter 0 list


node : String -> List (Node a) -> Node a
node title items =
    Button.init (Push (Menu (enum items))) title


leaf : String -> a -> Node a
leaf title msg =
    Button.init (Exit msg) title


embed : String -> Html a -> Node a
embed title content =
    Button.init (Push (Embed (Html.map Emit content))) title


pure : Model a -> ( Model a, Cmd (Msg a) )
pure model =
    ( model, Cmd.none )


send : Msg a -> Cmd (Msg a)
send msg =
    Task.succeed msg |> Task.perform identity


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg model =
    case msg of
        TransitionMsg msg ->
            case model.current of
                Transition { screens, transition } ->
                    let
                        nextTransition =
                            Transition.update msg transition
                    in
                        if Transition.isDone nextTransition then
                            pure { model | current = Single screens.to }
                        else
                            pure
                                { model
                                    | current =
                                        Transition
                                            { screens = screens
                                            , transition = nextTransition
                                            }
                                }

                Single _ ->
                    pure model

                None ->
                    pure model

        Push next ->
            pure
                (case model.current of
                    Single current ->
                        let
                            from t =
                                viewAnimatedScreen t current

                            to t =
                                viewAnimatedScreen t next
                        in
                            { model
                                | stack = current :: model.stack
                                , current =
                                    Transition
                                        { screens = { from = current, to = next }
                                        , transition = Transition.transition from to
                                        }
                            }

                    Transition _ ->
                        model

                    None ->
                        model
                )

        Pop ->
            case model.stack of
                [] ->
                    pure model

                last :: rest ->
                    let
                        previous =
                            screenMap (\( i, node ) -> ( i, Button.reset node )) last

                        current =
                            case model.current of
                                Single current ->
                                    let
                                        from t =
                                            viewAnimatedScreen t current

                                        to t =
                                            viewAnimatedScreen t previous
                                    in
                                        Transition
                                            { screens = { from = current, to = previous }
                                            , transition = Transition.transition from to
                                            }

                                Transition _ ->
                                    model.current

                                None ->
                                    model.current
                    in
                        pure
                            { model
                                | stack = rest
                                , current = current
                            }

        Exit msg ->
            pure
                (case model.current of
                    Single screen ->
                        { model
                            | current =
                                fadeOut screen (Emit msg)
                        }

                    _ ->
                        model
                )

        Emit _ ->
            pure model

        ButtonMsg id msg ->
            case model.current of
                Single screen ->
                    case Button.getParentMsg msg of
                        Just msg ->
                            update msg model

                        Nothing ->
                            let
                                f ( i, node ) =
                                    ( i
                                    , if id == i then
                                        Button.update msg node
                                      else
                                        node
                                    )
                            in
                                pure
                                    ({ model
                                        | current = Single (screenMap f screen)
                                     }
                                    )

                Transition _ ->
                    pure model

                None ->
                    pure model


subscriptions : Model a -> Sub (Msg a)
subscriptions model =
    case model.current of
        Transition { transition } ->
            Sub.map TransitionMsg (Transition.subscriptions transition)

        Single _ ->
            Sub.none

        None ->
            Sub.none


view : Model a -> Html (Msg a)
view { stack, current } =
    let
        back =
            case ( stack, current ) of
                ( _ :: _, Single _ ) ->
                    [ viewBackButton ]

                _ ->
                    []

        main =
            viewView current
    in
        div []
            [ div [ style (asPairs [ minHeight (Css.em 2) ]) ] back
            , main
            ]


viewView : View a -> Html (Msg a)
viewView view =
    let
        wrap =
            div [ style (asPairs [ position relative ]) ]
    in
        case view of
            Single screen ->
                wrap [ viewStaticScreen screen ]

            Transition { transition } ->
                Transition.view wrap transition

            None ->
                div [] []


viewStaticScreen : Screen a -> Html (Msg a)
viewStaticScreen screen =
    viewAnimatedScreen 1 screen


viewAnimatedScreen : Float -> Screen a -> Html (Msg a)
viewAnimatedScreen t screen =
    let
        styles =
            [ transform (scale (3 - (t * 2)))
            , opacity (num (t ^ 2))
            , position absolute
            , top zero
            , Css.width (pct 100)
            ]
    in
        viewScreenWithCss styles screen


viewScreenWithCss : List Css.Style -> Screen a -> Html (Msg a)
viewScreenWithCss styles screen =
    div [ style (asPairs styles) ] (viewScreen screen)


viewScreen : Screen a -> List (Html (Msg a))
viewScreen screen =
    case screen of
        Menu nodes ->
            let
                viewItem item =
                    div [ style (asPairs [ marginBottom (Css.em 1) ]) ]
                        [ viewButton item ]
            in
                List.map viewItem nodes

        Embed html ->
            [ html ]


viewButton : ( Int, Node a ) -> Html (Msg a)
viewButton ( i, node ) =
    let
        style =
            { defaultColor = hex "32cd32"
            , activeColor = hex "ee9a00"
            , disabledColor = hex "696969"
            }
    in
        div [] [ Html.map (ButtonMsg i) (Button.view style node) ]


viewBackButton : Html (Msg a)
viewBackButton =
    let
        styles =
            [ fontFamilies [ qt "Press Start 2P" ]
            , textTransform uppercase
            , textShadow4 (px 0) (px 1) (px 2) (rgb 0 0 0)
            , color (hex "FFFFFF")
            , backgroundColor transparent
            , border zero
            , cursor pointer
            ]
    in
        button
            [ style (asPairs styles)
            , onClick Pop
            ]
            [ Html.text "< back" ]
