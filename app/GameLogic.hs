module GameLogic where


import System.Random
import GameData
import Constants
-- Used structure from code linked in wiki; altered to fit our game (https://github.com/svolterra/SnakesandLadders)

-- takes an x and y and a array of ints and adds y to the value with index x
addAt :: Int -> Int -> [Int] -> [Int]
addAt x y = map (\(i,v) -> if (i==x) then y + v else v) . zip [0..]
-- takes an x and y and 2 array of ints representing the player pieces, and returns the pieces at that position in two arrays in a tuple
piecesAt :: Int -> Int -> [Int] -> [Int] -> ([Int],[Int])
piecesAt x y p1 p2 = 
    let p1Indexed = zip [0..] p1
        p2Indexed = zip [0..] p2
        p1Mapped = map (\(i,place) -> (i,player1Path!!place)) p1Indexed
        p2Mapped = map (\(i,place) -> (i,player2Path!!place)) p2Indexed
        p1Pieces = foldr (\(i,(py,px)) arr -> if ((x == px) && (y == py)) then arr ++ [i] else arr) [] p1Mapped
        p2Pieces = foldr (\(i,(py,px)) arr -> if ((x == px) && (y == py)) then arr ++ [i] else arr) [] p2Mapped
    in  (p1Pieces,p2Pieces)


-- Update the state of the player based on the number rolled by the dice
updatePlayerState :: PlayerState -> Int -> IO PlayerState
updatePlayerState (PlayerState {turn = t, player1Pieces = p1, player2Pieces = p2, diceRoll = d}) pieceToMove = do
    d <- randomRIO (1, 6)
    if t == 1
        then if (p1!!pieceToMove +d) <= 40
            then return PlayerState{turn = 2, player1Pieces = (addAt pieceToMove d p1) , player2Pieces = map (\a -> if (player2Path!!a ==(player1Path!!(p1!!pieceToMove +d))) then 0 else a ) p2, diceRoll = d}
                else return PlayerState{turn = 2, player1Pieces = p1 , player2Pieces = p2, diceRoll = d}
        else if t == 2
            then if (p2!!pieceToMove + d) <= 40
                then return PlayerState{turn = 1, player1Pieces = map (\a -> if (player1Path!!a ==(player2Path!!(p2!!pieceToMove +d))) then 0 else a ) p1 , player2Pieces = (addAt pieceToMove d p2), diceRoll = d}
                    else return PlayerState{turn = 1, player1Pieces = p1 , player2Pieces = p2, diceRoll = d}
            else return PlayerState{turn = t, player1Pieces = p1, player2Pieces = p2, diceRoll = d}

-- Updates the state of the game based on the player states
updateGameState :: PlayerState -> GameState -> GameState
updateGameState (PlayerState {turn = t, player1Pieces = p1, player2Pieces = p2}) (GameState {grid = g, gameOver = o}) =
    let grid = replicate gridSize $ replicate gridSize ([], [])
        updatedGrid = [[piecesAt x y p1 p2
                        | x <- [0..gridSize-1]]
                        | y <- [0..gridSize-1]]
        gameOver1 = foldr (\ place boolAcc -> (place == 40) && boolAcc) True p1
        gameOver2 = foldr (\ place boolAcc -> (place == 40) && boolAcc) True p2
        gameOver' = if gameOver1 then 1 else if gameOver2 then 2 else 0
    in GameState{grid = updatedGrid, gameOver = gameOver'}
