module Constants where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

-- Define the screen width of the game window
screenWidth :: Int 
screenWidth = 800

-- Define the screen height of the game window
screenHeight :: Int
screenHeight = 550

-- Define the background color of the game window screen 
screenBackgroundColor :: Color
screenBackgroundColor = makeColorI 250 240 230 0

-- Define the grid size 
gridSize :: Int
gridSize = 10

-- Define the grid cell width
cellWidth :: Float
cellWidth = 50

-- Define solid-color rectangles
filledSquare :: Picture
filledSquare = rectangleSolid cellWidth cellWidth

-- Define outline-only rectangles
outlinedSquare :: Picture 
outlinedSquare = rectangleWire cellWidth cellWidth

-- RollButton
rollButtonWidth :: Int
rollButtonWidth = 200

rollButtonHeight :: Int
rollButtonHeight = 100

rollButtonLeft :: Int
rollButtonLeft = -350

rollButtonRight :: Int
rollButtonRight = rollButtonLeft + rollButtonWidth

rollButtonBottom :: Int
rollButtonBottom = 100

rollButtonTop :: Int
rollButtonTop = rollButtonBottom + rollButtonHeight

-- The text displayed in the roll button
rollButtonText :: String
rollButtonText = "Press to Roll"

-- Color representing snakes on the board
snakesColor :: Color
snakesColor = dark red

-- Color representing player one
playerOneColor :: Color
playerOneColor = dark red

-- Color representing player two
playerTwoColor :: Color
playerTwoColor = dark cyan

-- Define a light black color for the grid
lightBlack :: Color
lightBlack = light(light black)

