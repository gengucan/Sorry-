module Constants where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

-- Added second roll button as well as Player paths to existing code linked in the wiki (https://github.com/svolterra/SnakesandLadders)

-- -- Define the screen width of the game window
screenWidth :: Int 
screenWidth = 800

-- -- Define the screen height of the game window
screenHeight :: Int
screenHeight = 550

-- -- Define the background color of the game window screen 
screenBackgroundColor :: Color
screenBackgroundColor = makeColorI 250 240 230 0

-- Define the grid size 
gridSize :: Int
gridSize = 10

-- -- Define the grid cell width
cellWidth :: Float
cellWidth = 50

-- -- Define solid-color rectangles
filledSquare :: Picture
filledSquare = rectangleSolid cellWidth cellWidth

-- -- Define outline-only rectangles
outlinedSquare :: Picture 
outlinedSquare = rectangleWire cellWidth cellWidth

-- -- Roll Button
rollButtonWidth1 :: Int
rollButtonWidth1 = 200

rollButtonHeight1 :: Int
rollButtonHeight1 = 100

rollButtonLeft1 :: Int
rollButtonLeft1 = -350

rollButtonRight1 :: Int
rollButtonRight1 = rollButtonLeft1 + rollButtonWidth1

rollButtonBottom1 :: Int
rollButtonBottom1 = 100

rollButtonTop1 :: Int
rollButtonTop1 = rollButtonBottom1 + rollButtonHeight1

-- Roll Button
rollButtonWidth2 :: Int
rollButtonWidth2 = 200

rollButtonHeight2 :: Int
rollButtonHeight2 = 100

rollButtonLeft2 :: Int
rollButtonLeft2 = -300

rollButtonRight2 :: Int
rollButtonRight2 = rollButtonLeft2 + rollButtonWidth2

rollButtonBottom2 :: Int
rollButtonBottom2 = 15

rollButtonTop2 :: Int
rollButtonTop2 = rollButtonBottom2 + rollButtonHeight2

-- -- The text displayed in the roll button
rollButtonText1 :: String
rollButtonText1 = "Press to Roll 1"

rollButtonText2 :: String
rollButtonText2 = "Press to Roll 2"

-- -- Color representing player one
playerOneColor :: Color
playerOneColor = dark red

-- -- Color representing player two
playerTwoColor :: Color
playerTwoColor = dark cyan

-- -- Define a light black color for the grid
lightBlack :: Color
lightBlack = light(light black)

-- Indexes for 2d array for the path of Player 1
player1Path :: [(Int,Int)]
player1Path = [(1,6),(0,6),(0,5),(0,4),(0,3),(0,2),(0,1),(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),(9,0),(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9),(8,9),(7,9),(6,9),(5,9),(4,9),(3,9),(2,9),(1,9),(0,9),(0,8),(0,7),(1,7),(2,7),(3,7),(4,7)]

-- Indexes for 2d array for the path of Player 2
player2Path :: [(Int,Int)]
player2Path = [(8,3),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9),(8,9),(7,9),(6,9),(5,9),(4,9),(3,9),(2,9),(1,9),(0,9),(0,8),(0,7),(0,6),(0,5),(0,4),(0,3),(0,2),(0,1),(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),(9,0),(9,1),(9,2),(8,2),(7,2),(6,2),(5,2)]
