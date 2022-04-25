{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

-- Returns Columns whose topmost row is still not filled
--possibleMoves :: Board -> [Column]

-- Update the Board
--makeMove :: Board -> Color -> Column -> Board

-- Check if a player of particular color has won
--checkWin :: Board -> Color -> Bool

-- Generate all possible next move positions
-- Uses possibleMoves and makeMove
--moves :: Board -> Color -> [Board]