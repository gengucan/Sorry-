module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random ( randomRIO )
import Data.List
import Constants
import GHC.IO
import GameLogic
import Gui
import GameData

-- Handles user input based on the coordinates of their mouse
-- If mouse coordinates lie within the bounds of a button image, handle button event:
  -- The Roll button updates the player state and game state
handleEvent :: Event -> (PlayerState, GameState) -> (PlayerState, GameState)
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (playerState, gameState)
  | buttonClickedRoll (round x, round y) = (playerState', gameState')
  | otherwise = (playerState, gameState)
  where 
    playerState' = unsafePerformIO $ updatePlayerState playerState
    gameState' = updateGameState playerState' gameState
    buttonClickedRoll (x', y') = x' > rollLeft && x' < rollRight && y' > rollBottom && y' < rollTop
    (rollLeft, rollRight, rollBottom, rollTop) = (rollButtonLeft, rollButtonRight, rollButtonBottom, rollButtonTop)
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