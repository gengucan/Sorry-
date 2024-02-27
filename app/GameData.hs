module GameData where

import Constants
import qualified Control.Applicative as Map

-- Used structure from code linked in wiki; altered to fit our game

-- Define the game state data type
-- grid stores pairs of Booleans representing each player's presence at a certain cell
-- gameOver is 1 if the game is over and player 1 won
--             2 if the game is over and player 2 won
--          or 0 if the game continues
data GameState = GameState {
    grid :: [[([Int],[Int])]],
    gameOver :: Int
} deriving (Eq, Show)

-- Define the player state data type
data PlayerState = PlayerState {
    turn :: Int,
    player1Pieces :: [Int],
    player2Pieces :: [Int],
    diceRoll :: Int
} deriving (Eq, Show)


-- Define the initial game state
initialGameState :: (PlayerState, GameState)
initialGameState = (
    (PlayerState {turn = 1, player1Pieces = [0,0], player2Pieces = [0,0], diceRoll = 0}),
    GameState {
    grid = replicate gridSize $ replicate gridSize ([],[]),
    gameOver = 0
})
