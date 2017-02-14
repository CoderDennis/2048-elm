module GameModel exposing (..)

import List exposing (..)
import List.Extra exposing (..)


type Direction
    = Up
    | Down
    | Left
    | Right


type Tile
    = Number Int
    | Empty


type Grid
    = Grid (List (List Tile))


type Progress
    = InProgress
    | GameOver
    | Won


type alias Model =
    { grid : Grid
    , score : Int
    , gameProgress : Progress
    }


gridSize : Int
gridSize =
    4


emptyGrid : Grid
emptyGrid =
    Grid <| List.repeat gridSize <| List.repeat gridSize <| Empty


initialModel : Model
initialModel =
    { grid = emptyGrid
    , score = 0
    , gameProgress = InProgress
    }


readTile : ( Int, Int ) -> Grid -> Maybe Tile
readTile ( i, j ) (Grid g) =
    let
        r =
            (g !! j)
    in
        case r of
            Just r ->
                (r !! i)

            Nothing ->
                Nothing


setTile : ( Int, Int ) -> Grid -> Tile -> Grid
setTile ( i, j ) (Grid g) t =
    case (g !! j) of
        Just r ->
            ((take i r) ++ [ t ] ++ (drop (i + 1) r))
                |> setRow j (Grid g)

        _ ->
            (Grid g)


setRow : Int -> Grid -> List Tile -> Grid
setRow j (Grid g) r =
    Grid <| (take j g) ++ [ r ] ++ (drop (j + 1) g)


tileToInt : Tile -> Int
tileToInt t =
    case t of
        Number n ->
            n

        otherwise ->
            0


intToTile : Int -> Tile
intToTile n =
    case n of
        0 ->
            Empty

        otherwise ->
            Number n


tilesWithCoordinates : Grid -> List ( Tile, Int, Int )
tilesWithCoordinates (Grid g) =
    concat <|
        Native.List.zipWith (\j r -> map (\( t, i ) -> ( t, i, j )) r)
            (range 0 (gridSize - 1))
        <|
            map (\r -> zip r (range 0 (gridSize - 1))) <|
                g


rotateGrid : Grid -> Grid
rotateGrid (Grid g) =
    Grid <| map reverse <| transpose g
