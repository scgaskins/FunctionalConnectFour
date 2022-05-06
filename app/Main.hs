{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import System.IO (hSetBuffering, BufferMode (NoBuffering), stdout)
import Text.Read (readMaybe)
import Board

players :: [Color]
players = [Red, Yellow]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Connect 4!"
  putStrLn "The objective of the game is to get "
  putStrLn "four in a row before the other player." 
  putStrLn "The board is 7 columns x 6 rows"
  putStrLn "Here is the starting board!"
  putStrLn "-------------------------------------------------"
  putStrLn ""
  putStrLn (formatBoard startingBoard) --print initial board to screen
  putStrLn "-------------------------------------------------"
  --play game with starting board and first player Red
  playGame startingBoard 0
    where startingBoard = emptyBoard 7 6
  
playGame :: Board -> Int -> IO ()
playGame board playerIndex
  | checkWin Red board    = putStrLn (formatColor Red :" Wins!!") 
  | checkWin Yellow board = putStrLn (formatColor Yellow : " Wins!!")
  | otherwise = do
      putStrLn ("Player " ++ show (playerIndex + 1) ++ " (" ++ formatColor (changePlayers players playerIndex) : "):")
      putStrLn "Enter a column number from "
      putStrLn "1 to 7 to place your piece: "
      playerColumnNum <- readPlayerInput (possibleMoves board)
      --update board with piece
      case makeMove (changePlayers players playerIndex) playerColumnNum board of
        Just newBoard -> do
          putStrLn "-------------------------------------------------"
          putStrLn ""
          putStrLn (formatBoard newBoard)
          putStrLn "-------------------------------------------------"
          playGame newBoard (update playerIndex)
        Nothing       -> do
          putStrLn "That is not a valid column"
          playGame board playerIndex

-- Given a list of valid inputs it reads in the user's
-- input and returns the input - 1 if it is a number in the
-- valid input list. Otherwise it prompts the user again
readPlayerInput :: [Int] -> IO Int
readPlayerInput validInputs = do
  playerInput <- getLine
  case (readMaybe playerInput :: Maybe Int) of
    Just i
      | (i-1) `elem` validInputs -> return (i-1)
      | otherwise                -> do
        putStrLn "That is not a valid column"
        readPlayerInput validInputs
    Nothing                      -> do
      putStrLn "That is not a number"
      readPlayerInput validInputs
