{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import System.IO (hSetBuffering, BufferMode (NoBuffering), stdout)
--import Move
--import Board

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Connect 4!!"
  putStrLn "The objective of the game is to "
  putStrLn "get four in a row before the computer." 
  putStrLn "The board is 7 columns x 6 rows"
  putStrLn "Here is the starting board!"
  --print initial board to screen
  --playGame board
  
-- playGame :: Board -> IO ()
-- playGame = do
--   putStrLn "Please enter a column number from 1 to 7 to "
--   putStrLn "place your piece: "
--   columnNum <- getLine
--   print columnNum --update board with piece
  --check if won
  --if not keep playing

--isWon :: Board -> IO ()
--isWon = do



-- main: make board and call event loop

-- event loop:

-- draw board
-- is it won?
-- print the win message
-- not won?
-- get a move
-- recursive call to event loop with updated board, next player