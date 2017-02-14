module Elm2048 exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import GameModel exposing (..)
import Rendering exposing (display)


--import Logic exposing (stepGame)


type Msg
    = NewGame
    | Move Direction


view : Model -> Html Msg
view model =
    div [ id "top" ]
        [ h1 [] [ text "2048-elm" ]
        , div [ id "sideBits" ]
            [ div [ id "scoreContainer" ]
                [ strong []
                    [ text ("SCORE " ++ (toString model.score)) ]
                ]
            , div
                [ id "newGameButton"
                , onClick NewGame
                ]
                [ strong []
                    [ text "New Game" ]
                ]
            , p [ id "description" ]
                [ text "Join the numbers and get to the "
                , strong [] [ text "2048 tile!" ]
                ]
            ]
        , div [ id "gameBoard" ]
            [ viewGameBoard model ]
        ]


viewGameBoard : Model -> Html Msg
viewGameBoard model =
    div [] []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( initialModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
