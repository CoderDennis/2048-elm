module Elm2048 exposing (..)

import Logic exposing (stepGame)
import Rendering exposing (display)


type Direction
    = Up
    | Down
    | Left
    | Right
    | None


type Tile
    = Number Int
    | Empty


type Grid
    = Grid List (List Tile)


type Progress
    = InProgress
    | GameOver
    | Won


type alias GameState =
    { grid : Grid
    , score : Int
    , gameProgress : Progress
    }


gridSize : Int
gridSize =
    4


emptyGrid : Grid
emptyGrid =
    Grid <| repeat gridSize <| repeat gridSize <| Empty


defaultGame : GameState
defaultGame =
    { grid = emptyGrid
    , score = 0
    , gameProgress = InProgress
    }
