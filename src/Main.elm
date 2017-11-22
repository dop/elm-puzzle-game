module Main exposing (..)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Puzzle
import Menu exposing (Msg, node, leaf, embed)
import Credits
import Scoreboard


type MenuCommand
    = Play Level
    | ShowScoreboard


type GoBack
    = GoBack


type Msg
    = MenuMsg (Menu.Msg MenuCommand)
    | PuzzleMsg Puzzle.Msg
    | ScoreboardMsg (Scoreboard.Msg GoBack)


type Model
    = Menu (Menu.Model MenuCommand)
    | Puzzle Puzzle.Model
    | Scoreboard (Scoreboard.Model GoBack)


type alias Level =
    { id : Int
    , size : Int
    , timeLimit : Int
    , name : String
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    menu


menu : ( Model, Cmd Msg )
menu =
    ( Menu
        (Menu.init
            [ Menu.node "Play"
                [ leaf "Easy" (Play levelEasy)
                , leaf "Medium" (Play levelMedium)
                , leaf "Hard" (Play levelHard)
                ]
            , leaf "Scoreboard" ShowScoreboard
            , Menu.embed "Credits" Credits.view
            ]
        )
    , Cmd.none
    )


scoreboard : ( Model, Cmd a )
scoreboard =
    let
        ( model, cmd ) =
            Scoreboard.init GoBack
    in
        ( Scoreboard (model), cmd )


puzzle : ( Model, Cmd Msg )
puzzle =
    let
        { size, timeLimit } =
            levelEasy

        ( puzzle, cmds ) =
            Puzzle.init size timeLimit
    in
        ( Puzzle puzzle, Cmd.map PuzzleMsg cmds )


levelEasy : Level
levelEasy =
    Level 1 4 60 "Easy"


levelMedium : Level
levelMedium =
    Level 2 5 120 "Medium"


levelHard : Level
levelHard =
    Level 3 6 210 "Hard"


levelImprobable : Level
levelImprobable =
    Level 4 8 300 "Improbable"


levelImpossible : Level
levelImpossible =
    Level 4 10 480 "Impossible"


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Puzzle puzzle ->
            Sub.map PuzzleMsg (Puzzle.subscriptions puzzle)

        Menu menu ->
            Sub.map MenuMsg (Menu.subscriptions menu)

        Scoreboard scores ->
            Sub.map ScoreboardMsg (Scoreboard.subscriptions scores)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MenuMsg msg ->
            case Menu.getParentMsg msg of
                Just msg ->
                    updateByMenu msg model

                Nothing ->
                    case model of
                        Menu model ->
                            let
                                ( menuModel, menuCmd ) =
                                    Menu.update msg model
                            in
                                ( Menu menuModel, Cmd.map MenuMsg menuCmd )

                        _ ->
                            ( model, Cmd.none )

        PuzzleMsg msg ->
            case model of
                Puzzle model ->
                    let
                        ( puzzle, cmds ) =
                            Puzzle.update msg model
                    in
                        ( Puzzle puzzle, Cmd.map PuzzleMsg cmds )

                _ ->
                    ( model, Cmd.none )

        ScoreboardMsg msg ->
            case Scoreboard.getParentMsg msg of
                Just GoBack ->
                    menu

                Nothing ->
                    case model of
                        Scoreboard model ->
                            ( Scoreboard (Scoreboard.update msg model), Cmd.none )

                        _ ->
                            ( model, Cmd.none )


updateByMenu : MenuCommand -> Model -> ( Model, Cmd Msg )
updateByMenu menu model =
    case menu of
        Play { size, timeLimit } ->
            let
                ( puzzle, cmds ) =
                    Puzzle.init size timeLimit
            in
                ( Puzzle puzzle, Cmd.map PuzzleMsg cmds )

        ShowScoreboard ->
            scoreboard


container : Int -> Html a -> Html a
container width html =
    let
        containerStyle =
            [ position absolute
            , left zero
            , top zero
            , right zero
            , bottom zero
            , backgroundColor (hex "202020")
            , textAlign center
            , color (hex "FFFFFF")
            ]

        wrapperStyle =
            [ margin2 zero auto
            , Css.width (px (toFloat width))
            ]
    in
        div [ style (asPairs containerStyle) ]
            [ div [ style (asPairs wrapperStyle) ]
                [ html ]
            ]


view : Model -> Html Msg
view model =
    let
        main =
            case model of
                Menu menu ->
                    Html.map MenuMsg (Menu.view menu)

                Puzzle puzzle ->
                    Html.map PuzzleMsg (Puzzle.view puzzle)

                Scoreboard scores ->
                    Html.map ScoreboardMsg (Scoreboard.view scores)
    in
        container 640
            (div []
                [ Html.h1 [] [ Html.text "Puzzl'em!" ]
                , main
                ]
            )
