module Constants where

-- import Graphics.Gloss
-- import Graphics.Gloss.Data.Color

-- -- Define the screen width of the game window
-- screenWidth :: Int 
-- screenWidth = 800

-- -- Define the screen height of the game window
-- screenHeight :: Int
-- screenHeight = 550

-- -- Define the background color of the game window screen 
-- screenBackgroundColor :: Color
-- screenBackgroundColor = makeColorI 250 240 230 0

-- Define the grid size 
gridSize :: Int
gridSize = 10

-- -- Define the grid cell width
-- cellWidth :: Float
-- cellWidth = 50

-- -- Define solid-color rectangles
-- filledSquare :: Picture
-- filledSquare = rectangleSolid cellWidth cellWidth

-- -- Define outline-only rectangles
-- outlinedSquare :: Picture 
-- outlinedSquare = rectangleWire cellWidth cellWidth

-- -- RollButton
-- rollButtonWidth :: Int
-- rollButtonWidth = 200

-- rollButtonHeight :: Int
-- rollButtonHeight = 100

-- rollButtonLeft :: Int
-- rollButtonLeft = -350

-- rollButtonRight :: Int
-- rollButtonRight = rollButtonLeft + rollButtonWidth

-- rollButtonBottom :: Int
-- rollButtonBottom = 100

-- rollButtonTop :: Int
-- rollButtonTop = rollButtonBottom + rollButtonHeight

-- -- The text displayed in the roll button
-- rollButtonText :: String
-- rollButtonText = "Press to Roll"

-- --ResetButton
-- resetButtonWidth :: Int
-- resetButtonWidth = 100

-- resetButtonHeight :: Int
-- resetButtonHeight = 50

-- resetButtonLeft :: Int
-- resetButtonLeft = -300

-- resetButtonRight :: Int
-- resetButtonRight = resetButtonLeft + resetButtonWidth

-- resetButtonBottom :: Int
-- resetButtonBottom = 15

-- resetButtonTop :: Int
-- resetButtonTop = resetButtonBottom + resetButtonHeight

-- -- The text displayed in the reset button
-- resetButtonText :: String
-- resetButtonText = "Reset"

-- -- The color of the reset button
-- resetButtonColor :: Color
-- resetButtonColor = light(light (dim red))

-- -- Color representing ladders on the board
-- laddersColor :: Color
-- laddersColor = dark(dark yellow)

-- -- Color representing snakes on the board
-- snakesColor :: Color
-- snakesColor = dark red

-- -- Color representing player one
-- playerOneColor :: Color
-- playerOneColor = dark green

-- -- Color representing player two
-- playerTwoColor :: Color
-- playerTwoColor = dark cyan

-- -- Define a light black color for the grid
-- lightBlack :: Color
-- lightBlack = light(light black)

-- Indexes for 2d array for the path of Player 1
player1Path :: [(Int,Int)]
player1Path = [(1,6),(0,6),(0,5),(0,4),(0,3),(0,2),(0,1),(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),(9,0),(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9),(8,9),(7,9),(6,9),(5,9),(4,9),(3,9),(2,9),(1,9),(0,9),(0,8),(0,7),(1,7),(2,7),(3,7),(4,7)]

-- Indexes for 2d array for the path of Player 2
player2Path :: [(Int,Int)]
player2Path = [(8,3),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9),(8,9),(7,9),(6,9),(5,9),(4,9),(3,9),(2,9),(1,9),(0,9),(0,8),(0,7),(0,6),(0,5),(0,4),(0,3),(0,2),(0,1),(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),(9,0),(9,1),(9,2),(8,2),(7,2),(6,2),(5,2)]