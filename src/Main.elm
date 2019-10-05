module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)



---- MODEL ----


type Piece
    = O
    | X


type Cell
    = Empty
    | Filled Piece


type GameResult
    = Incomplete
    | Winner (Maybe Piece)


type alias Position =
    Int


type alias Model =
    { turn : Piece
    , board : Board
    , result : GameResult
    }


type alias Board =
    Array Cell


type alias WinningState =
    List Position


initialModel : Model
initialModel =
    { turn = O
    , board = Array.initialize 9 (always Empty)
    , result = Incomplete
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


flip : Piece -> Piece
flip piece =
    case piece of
        O ->
            X

        X ->
            O


winningStates : List WinningState
winningStates =
    [ [ 0, 1, 2 ]
    , [ 3, 4, 5 ]
    , [ 6, 7, 8 ]
    , [ 0, 3, 6 ]
    , [ 1, 4, 7 ]
    , [ 2, 5, 8 ]
    , [ 0, 4, 8 ]
    , [ 2, 4, 6 ]
    ]


isWinningState : Cell -> Board -> WinningState -> Bool
isWinningState cell board winningState =
    winningState
        |> List.map (\p -> Array.get p board)
        |> List.all (\c -> c == Just cell)


gameProgress : Board -> GameResult
gameProgress board =
    let
        -- check if it is a tied game
        isTied =
            board
                |> Array.toList
                |> List.all (\c -> c /= Empty)

        -- check to see if X is any of the winning states
        xWins =
            winningStates
                |> List.filter (isWinningState (Filled X) board)
                |> List.length
                |> (<) 0

        -- check to see if O is any of the winning states
        oWins =
            winningStates
                |> List.filter (isWinningState (Filled O) board)
                |> List.length
                |> (<) 0
    in
    if isTied == True then
        Winner Nothing

    else if xWins == True then
        Winner (Just X)

    else if oWins == True then
        Winner (Just O)

    else
        Incomplete


type Msg
    = NoOp
    | PlacePiece Position
    | ResetBoard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlacePiece position ->
            let
                turn =
                    flip model.turn

                board =
                    Array.set position (Filled model.turn) model.board

                result =
                    gameProgress board
            in
            ( { model
                | turn = turn
                , board = board
                , result = result
              }
            , Cmd.none
            )

        ResetBoard ->
            init

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { board, result } =
    Html.section [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "section" ]
                [ div [ class "title" ] [ h1 [ class "title" ] [ text "Tic Tac Toe" ] ]
                , div [ class "board" ]
                    [ board
                        |> Array.indexedMap (cellView result)
                        |> Array.toList
                        |> div [ class "columns is-gapless is-multiline" ]
                    ]
                , div [ class "status" ]
                    [ Html.h3 [] [ text <| gameResultText result ]
                    , div [ class "columns" ]
                        [ div [ class "column" ]
                            [ button [ class "button is-secondary", onClick ResetBoard ]
                                [ text "Reset Game" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


cellView : GameResult -> Position -> Cell -> Html Msg
cellView progress position cell =
    button
        [ class "column is-one-third button is-primary"
        , disabled (cell /= Empty || progress /= Incomplete)
        , PlacePiece position |> onClick
        ]
        [ cell |> cellText |> text ]


cellText : Cell -> String
cellText cell =
    case cell of
        Filled O ->
            "O"

        Filled X ->
            "X"

        Empty ->
            ""


gameResultText : GameResult -> String
gameResultText result =
    case result of
        Incomplete ->
            ""

        Winner Nothing ->
            "Tied!"

        Winner (Just X) ->
            "Winner: X"

        Winner (Just O) ->
            "Winner: O"



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
