module Logic exposing (..)

import GameModel
    exposing
        ( Model
        , Tile
        , Number
        , Empty
        , defaultGame
        , emptyGrid
        , gridSize
        , Grid
        , setTile
        , readTile
        , tileToInt
        , intToTile
        , tilesWithCoordinates
        , rotateGrid
        , InProgress
        , GameOver
        , Won
        )


{------------------------------------------------------------------------------
                                Tile sliding
------------------------------------------------------------------------------}


{-| takes a list of values and 'slides' them to
the left, joining in lists pairs of adjacent
identical values.
-}
groupedByTwo : List a -> List (List a)
groupedByTwo l =
    case l of
        [ x ] ->
            [ [ x ] ]

        [ x, y ] ->
            if (x == y) then
                [ [ x, y ] ]
            else
                [ [ x ], [ y ] ]

        x :: y :: xs ->
            if (x == y) then
                ([ x, y ] :: (groupedByTwo xs))
            else
                ([ x ] :: (groupedByTwo (y :: xs)))

        otherwise ->
            []


{-| slides list of tiles to left,
    merging tiles where necessary,
    and returning a full list of
    four tiles, and the number of
    points gained
-}
slideRow : List Tile -> ( List Tile, Int )
slideRow r =
    let
        grouped =
            groupedByTwo <| filter (\t -> t /= Empty) r
    in
        ( take gridSize <|
            (map (intToTile . sum . (map tileToInt)) grouped)
                ++ repeat gridSize Empty
        , sum
            . (map tileToInt)
          <|
            concat <|
                filter (\x -> length x > 1) grouped
        )


{-| slide all of the rows of a grid
    in a certain direction
-}
slideGrid : Direction -> Grid -> ( Grid, Int )
slideGrid dir grid =
    if (dir == None) then
        ( grid, 0 )
    else
        let
            rotatedGrid =
                (case dir of
                    Down ->
                        rotateGrid

                    Right ->
                        rotateGrid . rotateGrid

                    Up ->
                        rotateGrid . rotateGrid . rotateGrid

                    otherwise ->
                        id
                )
                <|
                    grid

            rowsWithScores =
                map slideRow <|
                    (\(Grid h) -> h) <|
                        rotatedGrid

            slidRotatedGrid =
                Grid <| map fst rowsWithScores

            scoreGained =
                sum <| map snd rowsWithScores

            slidGrid =
                (case dir of
                    Up ->
                        rotateGrid

                    Right ->
                        rotateGrid . rotateGrid

                    Down ->
                        rotateGrid . rotateGrid . rotateGrid

                    otherwise ->
                        id
                )
                <|
                    slidRotatedGrid
        in
            ( slidGrid, scoreGained )


{-| push the tiles in the grid
    according to the direction in
    the input
-}
slideModel : Input -> Model -> Model
slideModel input gameState =
    let
        newGridScore =
            slideGrid input.controls.tilePushDirection gameState.grid
    in
        if (fst newGridScore == gameState.grid) then
            gameState
        else
            { gameState
                | grid = fst newGridScore
                , score = gameState.score + snd newGridScore
            }



{------------------------------------------------------------------------------
                            Win and loss conditions
------------------------------------------------------------------------------}


{-| check if none of the rows or columns of a grid
    can be slid in any direction
-}
gameLost : Grid -> Bool
gameLost g =
    let
        up =
            fst <| slideGrid Up g

        down =
            fst <| slideGrid Down g

        left =
            fst <| slideGrid Left g

        right =
            fst <| slideGrid Right g
    in
        and
            [ g /= emptyGrid
            , up == down
            , down == left
            , left == right
            , right == g
            ]


{-| checks if a 2048 tile present in the grid
-}
gameWon : Grid -> Bool
gameWon (Grid g) =
    0 /= (length <| filter (\t -> t == Number 2048) <| concat g)


{-| set a game to be at game over
-}
lose : Model -> Model
lose gameState =
    { gameState | gameProgress = GameOver }


{-| set a game to be won
-}
win : Model -> Model
win gameState =
    { gameState | gameProgress = Won }



{------------------------------------------------------------------------------
                             Random tile placement
------------------------------------------------------------------------------}


{-| the probability that a new tile is a 2.
    equivalently, the probability that a new
    tile is a 4
-}
tile2Probability : Float
tile2Probability =
    0.9


{-| based on a float that will be random,
    return a new tile
-}
newTile : Float -> Tile
newTile x =
    if (x < tile2Probability) then
        (Number 2)
    else
        (Number 4)


{-| a list of the coordinates of the empty
    tiles in a grid
-}
emptyTiles : Grid -> List ( Int, Int )
emptyTiles g =
    map (\( _, i, j ) -> ( i, j )) <|
        filter (\( t, _, _ ) -> t == Empty) <|
            tilesWithCoordinates g


{-| based on a float that will
    be random, return Just the
    coordinates of an empty tile in a
    grid if one exists, or Nothing if
    there are none
-}
newTileIndex : Float -> Grid -> Maybe ( Int, Int )
newTileIndex x g =
    let
        emptyTileIndices =
            emptyTiles g
    in
        case emptyTileIndices of
            [] ->
                Nothing

            otherwise ->
                Just <|
                    emptyTileIndices
                        ! (floor <| (toFloat <| length emptyTileIndices) * x)


{-| place a random
    tile into a random position
    in the grid
-}
placeRandomTile : Float -> Float -> Model -> Model
placeRandomTile float1 float2 gameState =
    let
        tileIndex =
            newTileIndex float1 gameState.grid
    in
        if (tileIndex == Nothing) then
            gameState
        else
            { gameState
                | grid =
                    setTile
                        (maybe ( 0, 0 ) id <| tileIndex)
                        gameState.grid
                    <|
                        newTile float2
            }


{-| generate a new game with two random
    starting tiles
-}
newGame : Input -> Model
newGame input =
    placeRandomTile (input.randomFloats ! 0) (input.randomFloats ! 1) <|
        placeRandomTile (input.randomFloats ! 2) (input.randomFloats ! 3) <|
            defaultGame



{------------------------------------------------------------------------------
                          Game stepping function

stepGame : Input -> Model -> Model -- game stepper that is called every
                                           -- time the input changes
stepGame input gameState =
    if | input.controls.newGameButtonPressed -- if the new game button is
                                             -- pressed
            -> newGame input                 -- then start a new game

       | gameState.gameProgress /= InProgress -- else if the game is not
                                              -- in progress
            -> gameState                      -- then do nothing

       | gameWon gameState.grid -- else if the grid contains a 2048 tile
            -> win gameState    -- then set the game to be won

       | gameLost gameState.grid -- else if the grid is completely fixed
            -> lose gameState    -- then set the game to be lost

       | input.controls.tilePushDirection /= None -- else if the controls
                                                  -- indicate a tile push
            -> let pushedState = slideModel input gameState -- then try to push
                                                -- the tiles in that direction
                in if (pushedState == gameState) then gameState
                                -- check whether the grid has actually changed
                    else placeRandomTile
                        (input.randomFloats ! 0) -- and if it has, place a new
                        (input.randomFloats ! 1) -- random tile
                        pushedState

       | otherwise -> gameState -- if for some reason none of the above
                                -- are true, do nothing
------------------------------------------------------------------------------}
