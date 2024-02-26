module Gui where

--import qualified Data.Map as Map
import Graphics.Gloss
import qualified Constants as C
import GameData

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
    translate (fromIntegral y * C.cellWidth) (fromIntegral x * C.cellWidth) $
    --translate (-C.cellWidth / 2) (-C.cellWidth / 2) $ -- Adjust for centering the text
    scale 0.1 0.1 $
    color clr $
    text txt

-- Take in a cell and update its color depending on the token(s) present
updateCellColor :: ([Int],[Int]) -> Int -> Int -> Picture 
updateCellColor (cell1, cell2) x y
  | length cell1 < 1 && length cell2 < 1 = color C.lightBlack C.outlinedSquare -- 
  | length cell1 == 1 && length cell2 < 1 = if cell1 !! 0 == 1
                                             then renderTextInCell y x "2" C.playerOneColor -- color C.playerOneColor C.filledSquare 
                                             else renderTextInCell y x "1" C.playerOneColor
  | length cell1 < 1 && length cell2 == 1 = if cell2 !! 0 == 1
                                             then renderTextInCell y x "2" C.playerTwoColor -- color C.playerTwoColor C.filledSquare 
                                             else renderTextInCell y x "1" C.playerTwoColor
  | length cell1 == 2 = renderTextInCell y x "1 | 2" C.playerOneColor
  | length cell2 == 2 = renderTextInCell y x "1 | 2" C.playerTwoColor
  | otherwise = color C.lightBlack C.outlinedSquare

-- Define the game board grid
gridPicture :: GameState -> PlayerState -> Picture
gridPicture gameState playerState = pictures
  [ -- Draw the grid
    translate (-100) (-250 + 25) $ pictures
      [ translate (fromIntegral x * C.cellWidth) (fromIntegral y * C.cellWidth) $
            updateCellColor (currentState !! y !! x) y x
          , translate ((-C.cellWidth/2) + 50 * fromIntegral x) (10 + 50 * fromIntegral y) $ scale 0.1 0.1 $ text ""
      ]
        | x <- [0..C.gridSize-1], y <- [0..C.gridSize-1]
  ]
  where
      currentState = grid gameState

-- Adjust button color to match current player 
changeButtonColor :: PlayerState -> Color
changeButtonColor playerState
  | turn playerState  == 1 = C.playerOneColor
  | otherwise = C.playerTwoColor

-- Define the roll button picture
rollButtonPicture1 :: PlayerState -> Picture
rollButtonPicture1 state = Pictures [
    Translate (-250) 150 $ color (changeButtonColor state) $ rectangleSolid  (fromIntegral C.rollButtonWidth1) (fromIntegral C.rollButtonHeight1),
    Translate (-340) 138 $ Scale 0.18 0.18 $ Text C.rollButtonText1
    ]

rollButtonPicture2 :: PlayerState -> Picture
rollButtonPicture2 state = Pictures [
    Translate (-250) 40 $ color (changeButtonColor state) $ rectangleSolid  (fromIntegral C.rollButtonWidth2) (fromIntegral C.rollButtonHeight2),
    Translate (-340) 35 $ Scale 0.18 0.18 $ Text C.rollButtonText2
    ]

-- Define the "Number Rolled" screen text
rollResultText :: Picture
rollResultText = Translate (-340) (-150) $ Scale 0.2 0.2 $ Text youRolled
  where youRolled = "Number Rolled:"

-- Adjust resut box outline to display who rolled which color
changeRollResultBoxOutline :: PlayerState -> Color
changeRollResultBoxOutline playerState
  | isGameStart playerState = white
  | turn playerState == 1 = C.playerTwoColor
  | otherwise = C.playerOneColor

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
  | winner == 1 = color C.playerOneColor $ rectangleSolid (fromIntegral C.screenWidth) (fromIntegral C.screenHeight)
  | otherwise = color C.playerTwoColor $ rectangleSolid (fromIntegral C.screenWidth) (fromIntegral C.screenHeight)
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
    | otherwise = Pictures [grid, rollButton1, rollButton2, result, resultText]
    where
        grid = gridPicture gameState playerState
        rollButton1 = rollButtonPicture1 playerState
        rollButton2 = rollButtonPicture2 playerState
        result = rollResultBox playerState
        resultText = rollResultText
        winnerText = declareWinner gameState