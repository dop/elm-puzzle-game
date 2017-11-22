port module Scoreboard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Css exposing (..)
import Transition exposing (TransitionData)


type alias Score =
    { name : String
    , score : Int
    }


type alias Model a =
    { scores : List Score
    , transition : Transition.Model (Msg a)
    }


type Msg a
    = TransitionMsg Transition.Msg
    | Scores (List Score)
    | Emit a


getParentMsg : Msg a -> Maybe a
getParentMsg msg =
    case msg of
        Emit msg ->
            Just msg

        _ ->
            Nothing



-- CONSTRUCTORS


init : a -> ( Model a, Cmd msg )
init msg =
    let
        scores =
            []
    in
        ( { scores = scores
          , transition = Transition.fadeIn (viewScores (viewBack msg) scores)
          }
        , getScores ()
        )



-- UPDATE


update : Msg a -> Model a -> Model a
update msg model =
    case msg of
        TransitionMsg msg ->
            { model | transition = Transition.update msg model.transition }

        Scores scores ->
            { model | scores = scores }

        Emit _ ->
            model


subscriptions : Model a -> Sub (Msg a)
subscriptions { transition } =
    Sub.batch
        [ Sub.map TransitionMsg (Transition.subscriptions transition)
        , scores Scores
        ]



-- VIEWS


view : Model a -> Html (Msg a)
view { transition } =
    Transition.view (div []) transition


viewBack : a -> Html (Msg a)
viewBack msg =
    div
        [ style
            (asPairs
                [ fontSize (px 10)
                , fontFamilies [ qt "Press Start 2P" ]
                ]
            )
        ]
        [ button [ onClick (Emit msg) ] [ t "< BACK" ]
        ]


viewScores : Html (Msg a) -> List Score -> Float -> Html (Msg a)
viewScores back scores time =
    let
        content =
            case scores of
                [] ->
                    [ h "No Scores Yet"
                    , p "Play a game to get some!"
                    ]

                scores ->
                    [ h "Scores"
                    , Html.table []
                        [ thead []
                            [ tr []
                                [ hcell ""
                                , hcell "NAME"
                                , hcell "SCORE"
                                ]
                            ]
                        , tbody []
                            (List.map
                                viewScore
                                (enum scores)
                            )
                        ]
                    ]
    in
        div
            [ style
                (asPairs
                    [ textAlign left
                    , opacity (num time)
                    ]
                )
            ]
            ([ back ] ++ content)


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


viewScore : ( Int, Score ) -> Html a
viewScore ( n, { name, score } ) =
    tr []
        [ cell (toString n ++ ".")
        , ncell (String.toUpper name)
        , scell (toString score)
        ]


hcell : String -> Html a
hcell text =
    th
        [ style
            (asPairs
                [ fontWeight normal
                , fontFamilies [ qt "Press Start 2P" ]
                ]
            )
        ]
        [ t text ]


ncell : String -> Html a
ncell text =
    td
        [ style
            (asPairs
                [ minWidth (px 400)
                , fontFamilies [ qt "Press Start 2P" ]
                ]
            )
        ]
        [ t text ]


scell : String -> Html a
scell text =
    td
        [ style
            (asPairs
                [ textAlign center
                , fontFamilies [ qt "Press Start 2P" ]
                ]
            )
        ]
        [ t text ]


cell : String -> Html a
cell text =
    td [ style (asPairs [ fontFamilies [ qt "Press Start 2P" ] ]) ]
        [ t text ]


h : String -> Html a
h text =
    h2 [] [ t text ]


p : String -> Html a
p text =
    Html.p [ style (asPairs [ textIndent (Css.em 1) ]) ]
        [ t text ]


t : String -> Html a
t text =
    Html.text text



-- PORTS


port getScores : () -> Cmd msg


port scores : (List Score -> msg) -> Sub msg
