module TicTacToe where

import Data.Maybe (isNothing)
import Data.List (intercalate, transpose)
import Text.Read (readMaybe)

-- Extract some magic literals to constants
boardSize = 3
colSep = "|"
-- We can generalize the row separator used in board printing to bigger boards
rowSep = "\n-" ++ (concat $ replicate (boardSize - 1) "+-") ++ "\n"

-- This condition gets checked a few times, nice to move it out
inRange :: Int -> Bool
inRange n = n >= 0 && n < boardSize

-- Instead of representing players with Strings, we can use effectively an enum
-- (with a different
data Player = X | O deriving (Eq, Show)

-- Nothing represents an empty board space, where Just _ represents a filled space
-- Nicer than working with strings, empowers ghc to tell us more accurately when things
-- are messed up
type Board = [[Maybe Player]]

defaultBoard :: Board
defaultBoard = replicate boardSize (replicate boardSize Nothing)

-- Used in printing the board
showCell :: Maybe Player -> String
showCell (Just X) = "X"
showCell (Just O) = "O"
showCell Nothing = " "

-- Only difference here is the additional map call, so we're working with strings
-- for the intercalate call
printBoard :: Board -> IO ()
printBoard board = do
  putStrLn $ intercalate rowSep (map  (\row -> intercalate colSep (map showCell row)) board)

isValidMove :: Board -> Int -> Int -> Bool
isValidMove board row col =
  let isInBounds = inRange row && inRange col
      -- We get to use isNothing instead of a string equality check here!
      isEmptySpace = isNothing (board !! row !! col)
  in isInBounds && isEmptySpace

makeMove :: Board -> Int -> Int -> Player -> Board
makeMove board row col player =
  zipWith
  (
    \rowIndex currRow ->
      zipWith
      (
        \colIndex val ->
          if row == rowIndex && col == colIndex
             then (Just player)
             else val
      )
      [0..]
      currRow
  )
  [0..]
  board


-- Use boardSize instead of 2 to generalize to bigger boards
checkWinner :: Board -> Player -> Bool
checkWinner board player =
  any (all (== (Just player))) board || -- Rows
  any (all (== (Just player))) (transpose board) || -- Columns
  all (== (Just player)) [board !! i !! i | i <- [0..(boardSize - 1)]] || -- Diagonal
  all (== (Just player)) [board !! i !! (boardSize - i - 1) | i <- [0..(boardSize - 1)]] -- Anti-diagonal

-- Need to check for ties too! Board can fill up without a winner
-- Without this check, we'd be stuck in an infinite loop once that happens
-- We don't need to check for winners in THIS function because
-- calls to this function happy strictly after checkWinner
-- Note: draws *could* be determined earlier (when no winning plays are possible)
-- but this check suffices to prevent the non-termination issue
checkDraw :: Board -> Bool
checkDraw board =
  all (all (/= Nothing)) board

-- Convenience for switching players post-move
nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

-- Help with parsing user input, wrapped in Maybe to allow malformed input without crashing
parseInput :: String -> Maybe (Int, Int)
parseInput s = case words s of
  [row, col] -> do
    row' <- readMaybe row
    col' <- readMaybe col
    if inRange row' && inRange col' then Just (row', col') else Nothing
  _ -> Nothing

-- We can inject our own monads for testing this way, so we don't have to try to spoof user input
playGameGeneral :: Monad m => m String -> (String -> m ()) -> (Board -> m ()) -> Board -> Player -> m ()
playGameGeneral inputReader outputWriter boardPrinter board currentPlayer = do
  boardPrinter board
  outputWriter $ "Player " ++ show currentPlayer ++ ", enter your move (row and column separated by a space):"
  input <- inputReader
  case parseInput input of
    -- Can use guarded case to simplify validation logic a bit
    Just (row, col) | isValidMove board row col -> do
      let newBoard = makeMove board row col currentPlayer
      if checkWinner newBoard currentPlayer
        then do
          boardPrinter newBoard
          outputWriter $ "Player " ++ show currentPlayer ++ " wins!"
      -- Check for draws
      else if checkDraw newBoard
        then do
          boardPrinter newBoard
          outputWriter "It's a draw!"
      else playGameGeneral inputReader outputWriter boardPrinter newBoard (nextPlayer currentPlayer)
    Just _ -> do
      outputWriter "Invalid move, cell is occupied. Try again"
      playGameGeneral inputReader outputWriter boardPrinter board currentPlayer
    Nothing -> do
      outputWriter $ "Invalid Input. Please enter two numbers between 0 and " ++ show (boardSize - 1) ++ ", separated by a space."
      playGameGeneral inputReader outputWriter boardPrinter board currentPlayer


playGame :: Board -> Player -> IO ()
playGame board currentPlayer = do
  printBoard board
  putStrLn $ "Player " ++ show currentPlayer ++ ", enter your move (row and column separated by a space):"
  input <- getLine
  case parseInput input of
    -- Can use guarded case to simplify validation logic a bit
    Just (row, col) | isValidMove board row col -> do
      let newBoard = makeMove board row col currentPlayer
      if checkWinner newBoard currentPlayer
        then do
          printBoard newBoard
          putStrLn $ "Player " ++ show currentPlayer ++ " wins!"
      -- Check for draws
      else if checkDraw newBoard
        then do
          printBoard newBoard
          putStrLn "It's a draw!"
      else playGame newBoard (nextPlayer currentPlayer)
    Just _ -> do
      putStrLn "Invalid move, cell is occupied. Try again"
      playGame board currentPlayer
    Nothing -> do
      putStrLn $ "Invalid Input. Please enter two numbers between 0 and " ++ show (boardSize - 1) ++ ", separated by a space."
      playGame board currentPlayer
