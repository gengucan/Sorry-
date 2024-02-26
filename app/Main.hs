{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
--import System.Random (randomRIO)
--import Data.List
import Constants
import GameLogic
import Gui
import GameData
import GHC.IO

-- Handles user input based on the coordinates of their mouse
handleEvent :: Event -> (PlayerState, GameState) -> (PlayerState, GameState)
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (playerState, gameState)
  | buttonClickedRoll1 (round x, round y) = (playerState'1, gameState'1)
  | buttonClickedRoll2 (round x, round y) = (playerState'2, gameState'2)
  | otherwise = (playerState, gameState)
  where 
    playerState'1 = unsafePerformIO $ updatePlayerState playerState 1
    playerState'2 = unsafePerformIO $ updatePlayerState playerState 2
    gameState'1 = updateGameState playerState'1 gameState
    gameState'2 = updateGameState playerState'2 gameState
    buttonClickedRoll1 (x', y') = x' > rollLeft1 && x' < rollRight1 && y' > rollBottom1 && y' < rollTop1
    buttonClickedRoll2 (x', y') = x' > rollLeft2 && x' < rollRight2 && y' > rollBottom2 && y' < rollTop2
    (rollLeft1, rollRight1, rollBottom1, rollTop1) = (rollButtonLeft1, rollButtonRight1, rollButtonBottom1, rollButtonTop1)
    (rollLeft2, rollRight2, rollBottom2, rollTop2) = (rollButtonLeft2, rollButtonRight2, rollButtonBottom2, rollButtonTop2)
handleEvent _ (playerState, gameState) = (playerState, gameState)

-- Define the update function
update :: Float -> (PlayerState, GameState) -> (PlayerState, GameState)
update _ = id

-- Starts game in a new window
main :: IO ()
main = play
  (InWindow "Sorry!" (800, 550) (10, 10))
  screenBackgroundColor
  60
  initialGameState
  render
  handleEvent
  update