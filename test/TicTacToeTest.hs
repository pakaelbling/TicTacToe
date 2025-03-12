module Main where

import TicTacToe
import Test.HUnit
import System.IO
import System.IO.Silently (hCapture_)
import qualified System.Exit as Exit

tInRange0 = TestCase (assertBool "inRange0" (inRange 0))
tInRange2 = TestCase (assertBool "inRange2" (inRange 2))
tInRange100 = TestCase (assertBool "inRange100" (not $ inRange 100))
tInRange = TestList [
  TestLabel "tInRange0" tInRange0,
  TestLabel "tInRange2" tInRange2,
  TestLabel "tInRange100" tInRange100
  ]


tDefaultBoard = TestCase (
    assertEqual
    "defaultBoard"
    defaultBoard
    [[Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing]]
  )


tShowCellJX = TestCase (assertEqual "showCellJX" (showCell $ Just X) "X")
tShowCellJO = TestCase (assertEqual "showCellJO" (showCell $ Just O) "O")
tShowCellN = TestCase (assertEqual "showCellN" (showCell Nothing) " ")
tShowCell = TestList [
  TestLabel "showCellJX" tShowCellJX,
  TestLabel "showCellJO" tShowCellJO,
  TestLabel "showCellN" tShowCellN
  ]


tPrintDefaultBoard = TestCase $ do
  out <- hCapture_ [stdout] (printBoard defaultBoard)
  assertEqual "Expected output" " | | \n-+-+-\n | | \n-+-+-\n | | \n" out
tPrintMixedBoard = TestCase $ do
  let board = [
        [Just X, Nothing, Just O],
        [Nothing, Nothing, Nothing],
        [Just O, Nothing, Just X]
        ]
  out <- hCapture_ [stdout] (printBoard board)
  assertEqual "Expected output" "X| |O\n-+-+-\n | | \n-+-+-\nO| |X\n" out
tPrintBoard = TestList [
  TestLabel "printBoardDefault" tPrintDefaultBoard,
  TestLabel "printBoardMixed" tPrintMixedBoard
  ]


tIsValidMoveEmpty = TestCase (assertBool "isValidMoveEmpty" (isValidMove defaultBoard 0 0))
tIsValidMoveNonEmpty =
  let board = [
        [Just X, Nothing, Just O],
        [Nothing, Nothing, Nothing],
        [Just O, Nothing, Just X]
        ]
  in
    TestCase (assertBool "isValidMoveNonEmpty" (isValidMove board 1 1))
tIsValidMoveOccupied =
  let board = [
        [Just X, Nothing, Just O],
        [Nothing, Nothing, Nothing],
        [Just O, Nothing, Just X]
        ]
  in
    TestCase (assertBool "isValidMoveNonEmpty" (not $ isValidMove board 0 0))
tIsValidMoveOutOfBoundsRow = TestCase (assertBool "isValidMoveEmpty" (not $ isValidMove defaultBoard 1000 1))
tIsValidMoveOutOfBoundsCol = TestCase (assertBool "isValidMoveEmpty" (not $ isValidMove defaultBoard 1 1000))
tIsValidMove = TestList [
  TestLabel "isValidMoveEmptyBoard" tIsValidMoveEmpty,
  TestLabel "isValidMoveNonEmptyBoard" tIsValidMoveNonEmpty,
  TestLabel "isValidMoveOccupiedCell" tIsValidMoveOccupied,
  TestLabel "isValidMoveOutOfBoundsRow" tIsValidMoveOutOfBoundsRow,
  TestLabel "isValidMoveOutOfBoundsCol" tIsValidMoveOutOfBoundsCol
  ]


-- Validity checks happen in playGame, not in makeMove
tMakeMove = let expectedBoard = [
                  [Just O, Nothing, Nothing],
                  [Nothing, Nothing, Nothing],
                  [Nothing, Nothing, Nothing]
                  ]
            in
              TestCase (assertEqual "makeMove" (makeMove defaultBoard 0 0 O) expectedBoard)


tCheckWinnerNoWinner = TestCase (assertBool "checkWinnerNoWinner" (not $ checkWinner defaultBoard O))
tCheckWinnerWrongPlayer = let winningBoard = [
                                [Just O, Just O, Just O],
                                [Just X, Just O, Nothing],
                                [Nothing, Nothing, Nothing]
                                ]
                          in
                            TestCase (assertBool "checkWinnerWrongPlayer" (not $ checkWinner winningBoard X))
tCheckWinnerRow = let winningBoard = [
                        [Just O, Just O, Just O],
                        [Just X, Just O, Nothing],
                        [Nothing, Nothing, Nothing]
                        ]
                   in
                     TestCase (assertBool "checkWinnerRow" (checkWinner winningBoard O))
tCheckWinnerCol = let winningBoard = [
                          [Just O, Just O, Just X],
                          [Just O, Just O, Nothing],
                          [Just O, Nothing, Nothing]
                          ]
                  in
                    TestCase (assertBool "checkWinnerCol" (checkWinner winningBoard O))
tCheckWinnerDiag = let winningBoard = [
                        [Just O, Just O, Just X],
                        [Just X, Just O, Nothing],
                        [Nothing, Nothing, Just O]
                        ]
                   in
                     TestCase (assertBool "checkWinnerDiag" (checkWinner winningBoard O))
tCheckWinnerAntiDiag = let winningBoard = [
                             [Just O, Just O, Just X],
                             [Just X, Just X, Nothing],
                             [Just X, Nothing, Just O]
                             ]
                       in
                         TestCase (assertBool "checkWinnerAntiDiag" (checkWinner winningBoard X))
tCheckWinner = TestList [
  TestLabel "checkWinnerNoWinner" tCheckWinnerNoWinner,
  TestLabel "checkWinnerWrongPlayer" tCheckWinnerWrongPlayer,
  TestLabel "checkWinnerRow" tCheckWinnerRow,
  TestLabel "checkWinnerCol" tCheckWinnerCol,
  TestLabel "checkWinnerDiag" tCheckWinnerDiag,
  TestLabel "checkWinnerAntiDiag" tCheckWinnerAntiDiag
  ]


tCheckDrawDraw = let drawingBoard = [
                       [Just X, Just X, Just O],
                       [Just O, Just O, Just X],
                       [Just X, Just O, Just X]
                                    ]
                 in
                   TestCase (assertBool "checkDrawDraw" (checkDraw drawingBoard))
tCheckDrawNoDraw = TestCase (assertBool "checkDrawNoDraw" (not $ checkDraw defaultBoard))
tCheckDraw = TestList [
  TestLabel "checkDrawDraw" tCheckDrawDraw,
  TestLabel "checkDrawNoDraw" tCheckDrawNoDraw
  ]


tNextPlayerX = TestCase (assertEqual "nextPlayerX" (nextPlayer X) O)
tNextPlayerO = TestCase (assertEqual "nextPlayerO" (nextPlayer O) X)
tNextPlayer = TestList [
  TestLabel "nextPlayerX" tNextPlayerX,
  TestLabel "nextPlayerO" tNextPlayerO
  ]


tParseInputSuccess = TestCase (assertEqual "parseInputSuccess" (parseInput "0 0") (Just (0, 0)))
tParseInputFail = TestCase (assertEqual "parseInputFail" (parseInput "foo") Nothing)
tParseInput = TestList [
  TestLabel "parseInputSuccess" tParseInputSuccess,
  TestLabel "parseInputFail" tParseInputFail
  ]


tests = TestList [
  TestLabel "inRange" tInRange,
  TestLabel "defaultBoard" tDefaultBoard,
  TestLabel "showCell" tShowCell,
  TestLabel "printBoard" tPrintBoard,
  TestLabel "isValidMove" tIsValidMove,
  TestLabel "makeMove" tMakeMove,
  TestLabel "checkWinner" tCheckWinner,
  TestLabel "checkDraw" tCheckDraw,
  TestLabel "nextPlayer" tNextPlayer,
  TestLabel "parseInput" tParseInput
                 ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
