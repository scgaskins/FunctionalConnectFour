{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import System.IO (hSetBuffering, BufferMode (NoBuffering), stdout)

main :: IO ()
main =
  hSetBuffering stdout NoBuffering *>
  putStrLn "Welcome to Connect 4!!" *>
  putStrLn "The objective of the game is to simply get "*>
  putStrLn "four in a row before the other player. Have fun!" *>
  putStr "Please enter your name: " *>
  getLine >>= \name ->
  putStrLn ("Welcome, " ++ name ++ "!")
  --putStr "Please enter a number: " *>
  --readLn >>= \n ->
  --print (n :: Int)

-- main: make board and call event loop

-- event loop:

-- draw board
-- is it won?
-- print the win message
-- not won?
-- get a move
-- recursive call to event loop with updated board, next player