module Main exposing (..)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Puzzle
import Menu exposing (Msg, node, leaf)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type MenuCommand
    = Play Level
    | ShowCredits
    | Scoreboard


type Msg
    = MenuMsg (Menu.Msg MenuCommand)
    | PuzzleMsg Puzzle.Msg


type Model
    = Menu (Menu.Model MenuCommand)
    | Puzzle Puzzle.Model


type alias Level =
    { id : Int
    , size : Int
    , timeLimit : Int
    , name : String
    }


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


defaultModel : Model
defaultModel =
    Menu
        (Menu.init
            [ Menu.node "Play"
                [ leaf "Easy" (Play levelEasy)
                , leaf "Medium" (Play levelMedium)
                , leaf "Hard" (Play levelHard)
                ]
            , leaf "Scoreboard" Scoreboard
            , leaf "Credits" ShowCredits
            ]
        )


init : ( Model, Cmd Msg )
init =
    ( defaultModel
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Puzzle puzzle ->
            Sub.map PuzzleMsg (Puzzle.subscriptions puzzle)

        Menu menu ->
            Sub.map MenuMsg (Menu.subscriptions menu)


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


updateByMenu : MenuCommand -> Model -> ( Model, Cmd Msg )
updateByMenu menu model =
    case menu of
        Play { size, timeLimit } ->
            let
                ( puzzle, cmds ) =
                    Puzzle.init size timeLimit
            in
                ( Puzzle puzzle, Cmd.map PuzzleMsg cmds )

        ShowCredits ->
            ( model, Cmd.none )

        Scoreboard ->
            ( model, Cmd.none )


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
    in
        container 640
            (div []
                [ Html.h1 [] [ Html.text "Puzzl'em!" ]
                , main
                ]
            )
