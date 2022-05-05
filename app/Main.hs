{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import System.IO (hSetBuffering, BufferMode (NoBuffering), stdout)
import Board

players = [Red, Yellow]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Connect 4!"
  putStrLn "The objective of the game is to get "
  putStrLn "four in a row before the other player." 
  putStrLn "The board is 7 columns x 6 rows"
  putStrLn "Here is the starting board!"
  print startingBoard --print initial board to screen
  --play game with starting board and first player Red
  playGame startingBoard 0
    where startingBoard = emptyBoard 6 7
  
playGame :: Board -> Int -> IO ()
playGame board playerIndex
  | checkWin Red board    = putStrLn "Red Wins!!" 
  | checkWin Yellow board = putStrLn "Yellow Wins!!"
  | otherwise = do
      putStrLn ("Player " ++ show playerIndex ++ "(" ++ show (changePlayers players playerIndex) ++ "):")
      putStrLn "Enter a column number "
      putStrLn "from 1 to 7 to place your piece: "
      playerColumnNum <- getLine
      --update board with piece
      case (makeMove (changePlayers players playerIndex) playerColumnNum board) of
        Just newBoard -> do
          print newBoard
          playGame newBoard (update playerIndex)
        Nothing       -> do
          putStrLn "That is not a valid column"
          playGame board playerIndex
      -- print board
      -- playGame board 
      --   where newBoard = 
  

-- event loop:

-- draw board
-- is it won? (done)
-- print the win message
-- not won? (done)
-- get a move (done)
-- recursive call to event loop with updated board