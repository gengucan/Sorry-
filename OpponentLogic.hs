module OpponentLogic where

import GameLogic
import GameData
import System.Random

-- Defining the computer opponent 
data ComputerStrategy = RandomMove deriving (Eq, Show)

-- Helper function that generate a random move for the computer player opponent
generateRandomMove :: Int -> PlayerState -> (Int, PlayerState)
generateRandomMove selectedPiece playerState = (newDiceRoll, updatedPlayerState)
    where
        (newDiceRoll, _) = randomR (1, 6) (mkStdGen 42) -- i might need a random generator setup, this current one provides the same random sequence with sed 42
        updatedPlayerState = updatePlayerState playerState selectedPiece

-- A function to handle the opponent's move
opponentMove :: PlayerState -> GameState -> (PlayerState, GameState)
opponentMove playerState gameState =
    case computerStrategy playerState of
        RandomMove -> do
            let availablePieces = findAvailablePieces playerState
            let selectedPiece = if null availablePieces then 0 else head availablePieces
            let (newDiceRoll, updatedPlayerState) = generateRandomMove selectedPiece playerState
            let gameState' = updateGameState updatedPlayerState gameState
            (updatedPlayerState { diceRoll = newDiceRoll }, gameState')

            