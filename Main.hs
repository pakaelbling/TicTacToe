module Main where

import TicTacToe


main :: IO ()
main = do
  putStrLn "Welcome to Phil's Tic Tac Toe!"
  playGame defaultBoard X
