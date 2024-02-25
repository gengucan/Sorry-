module Gui where

--import qualified Data.Map as Map
import Graphics.Gloss
import Constants

-- Determine if the game has just started
-- Return true if yes, false otherwise
isGameStart :: PlayerState -> Bool
isGameStart state = p1 == [17,17] && p2 == [84,84]
  where
    p1 = player1Pieces state
    p2 = player2Pieces state

-- Function to render text in a specific cell of the grid
renderTextInCell :: Int -> Int -> String -> Color -> Picture
renderTextInCell x y txt clr =
    translate (fromIntegral x * cellWidth) (fromIntegral y * cellWidth) $
    translate (-cellWidth / 2) (-cellWidth / 2) $ -- Adjust for centering the text
    scale 0.1 0.1 $ -- Adjust the text size as needed
    color clr $ -- Set text color - need to set to match the player
    text txt

-- take in cell; needs to account for overlap? - do we trigger the overlap check here?
updateCellColor :: ([Int],[Int]) -> Int -> Int -> Picture 
updateCellColor cell x y
  | cell !! 1 !! 1 != 0 = color playerOneColor filledSquare
    if cell !! 1 !! 1 == 1 then renderTextInCell x y "1" playerOneColor else renderTextInCell x y "2" playerOneColor
  | cell !! 2 !! 1 != 0 = color playerTwoColor filledSquare
    if cell !! 2 !! 1 == 1 then renderTextInCell x y "1" playerTwoColor else renderTextInCell x y "2" playerTwoColor
  | otherwise = color lightBlack outlinedSquare

-- Define the game board grid
gridPicture :: GameState -> PlayerState -> Picture
gridPicture gameState playerState = pictures
  [ -- Draw the grid
    translate (-100) (-250 + 25) $ pictures
      [ translate (fromIntegral x * cellWidth) (fromIntegral y * cellWidth) $
            updateCellColor (currentState !! y !! x) y x
          , translate ((-cellWidth/2) + 50 * fromIntegral x) (10 + 50 * fromIntegral y) $ scale 0.1 0.1
      ]
        | x <- [0..gridSize-1], y <- [0..gridSize-1]
  ]
  where
      currentState = grid gameState

-- Adjust button color to match current player 
changeButtonColor :: PlayerState -> Color
changeButtonColor playerState
  | turn playerState  == 1 = playerOneColor
  | otherwise = playerTwoColor

-- Define the roll button picture
rollButtonPicture :: PlayerState -> Picture
rollButtonPicture state = Pictures [
    Translate (-250) 150 $ color (changeButtonColor state) $ rectangleSolid  (fromIntegral rollButtonWidth) (fromIntegral rollButtonHeight),
    Translate (-328) 138 $ Scale 0.18 0.18 $ Text rollButtonText
    ]

-- Define the "Number Rolled" screen text
rollResultText :: Picture
rollResultText = Translate (-340) (-150) $ Scale 0.2 0.2 $ Text youRolled
  where youRolled = "Number Rolled:"

-- Adjust resut box outline to display who rolled which color
changeRollResultBoxOutline :: PlayerState -> Color
changeRollResultBoxOutline playerState
  | isGameStart playerState = white
  | turn playerState == 1 = playerTwoColor
  | otherwise = playerOneColor

-- Define the box representing the rolled number
rollResultBox :: PlayerState -> Picture
rollResultBox playerState = Pictures [
    Translate (-250) (-200) $ color (changeRollResultBoxOutline playerState) $ rectangleSolid 80 50,
    Translate (-260) (-210) $ Scale 0.2 0.2 $ color black $ Text result
    ]
 where
     result = show (diceRoll playerState)

-- Determine if the game is over 
isGameOver :: GameState -> Bool
isGameOver gameState = winner == 1 || winner == 2
  where winner = gameOver gameState

-- Render winning screen with name and color of winner
renderWinningScreen :: GameState -> Picture
renderWinningScreen state
  | winner == 1 = color playerOneColor $ rectangleSolid (fromIntegral screenWidth) (fromIntegral screenHeight)
  | otherwise = color playerTwoColor $ rectangleSolid (fromIntegral screenWidth) (fromIntegral screenHeight)
  where
  winner = gameOver state

-- Render text mentioning the game winner
declareWinner :: GameState -> Picture
declareWinner state
  | winner == 1 = Translate (-50) 25 $ Scale 0.3 0.3 $ Text "Red Wins!"
  | otherwise = Translate (-50) 25 $ Scale 0.3 0.3  $ Text "Blue Wins!"
  where
    winner = gameOver state
    
-- Render the gameboard's components
render :: (PlayerState, GameState) -> Picture
render (playerState, gameState)
    | isGameOver gameState = Pictures [renderWinningScreen gameState, winnerText]
    | otherwise = Pictures [grid, rollButton, result, resultText]
    where
        grid = gridPicture gameState playerState
        rollButton = rollButtonPicture playerState
        result = rollResultBox playerState
        resultText = rollResultText
        winnerText = declareWinner gameState