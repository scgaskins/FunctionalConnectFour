{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Board where

import Data.List (group, transpose)
--rows = 6
--columns = 7

type Column = Int
type Row = Int
type Coords = (Column, Row)

-- Enumeration for the 3 Colors
data Color = Empty | Red | Yellow
    deriving (Eq, Show)

-- Board is just a 2 Dimensional List
type Board = [[Color]]

-- Returns a board full of Empty spaces
emptyBoard :: Int -> Int -> Board
emptyBoard columns rows = replicate columns (replicate rows Empty)


-- Returns Columns whose topmost row is still not filled
possibleMoves :: Board -> [Column]
possibleMoves = map fst . filter (\(_,color) -> color == Empty). zip (iterate (+1) 0) . map head

-- Adds the piece to the given column, returns Nothing if that
-- column is full
makeMove :: Color -> Column -> Board -> Maybe Board
makeMove _ _ []         = Nothing
makeMove color 0 (c:cs) = fmap (: cs) (addToColumn color c)
makeMove color n (c:cs) = fmap (c :) (makeMove color (n-1) cs)

-- Adds a piece to a column, replacing the last consecutive Empty
-- space. If there are no Empty spaces at the top, it returns Nothing
addToColumn :: Color -> [Color] -> Maybe [Color]
addToColumn Empty c                           = Just c
addToColumn color [Empty]                     = Just [color]
addToColumn color (Empty : nextColor : cs)
    | nextColor == Red || nextColor == Yellow = Just (color : nextColor : cs)
    | nextColor == Empty                      = fmap (Empty :) (addToColumn color (nextColor : cs))
addToColumn _ _                               = Nothing

-- Check if a player of particular color has won
checkWin :: Color -> Board -> Bool
checkWin c b = winByDiagonals c b || winByColumns c b || winByDiagonals c b

-- Checks if there are four in a row of the color in any diagonals
winByDiagonals :: Color -> Board -> Bool
winByDiagonals color = any (fourInARow color) . allDiagonals

-- Checks if there are four in a row of the color in any columns
winByColumns :: Color -> Board -> Bool
winByColumns color = any (fourInARow color) 

-- Checks if there are four in a row of the color in any rows
winByRows :: Color -> Board -> Bool
winByRows color = winByColumns color . transpose

-- Checks if there are four pieces of a given color in the list
fourInARow :: Color -> [Color] -> Bool
fourInARow color colors = longestSequence color colors >= 4

-- Finds the length of the longest sequence of a given element in the list
longestSequence :: Eq a => a -> [a] -> Int
longestSequence a = foldr (max . length) 0 . filter (a `elem`) . group

-- Generate all possible next move positions
-- Uses possibleMoves and makeMove
--moves :: Board -> Color -> [Board]

allDiagonals :: Board -> [[Color]]
allDiagonals b = allLeftToRightDiags b ++ allRightToLeftDiags b

allRightToLeftDiags :: Board -> [[Color]]
allRightToLeftDiags b = rightToLeftDiagsCDecr len wi b ++ rightToLeftDiagsRDecr len wi b
    where (len, wi) = (length b - 1,length (head b) - 1)

rightToLeftDiagsCDecr :: Column -> Row -> Board -> [[Color]]
rightToLeftDiagsCDecr c r b
    | c < 0 || r < 0 = []
    | otherwise      = rightToLeftDiagonal c r b : rightToLeftDiagsCDecr (c-1) r b

rightToLeftDiagsRDecr :: Column -> Row -> Board -> [[Color]]
rightToLeftDiagsRDecr c r b
    | c < 0 || r < 0 = []
    | otherwise      = rightToLeftDiagonal c r b : rightToLeftDiagsRDecr c (r-1) b

rightToLeftDiagonal :: Column -> Row -> Board -> [Color]
rightToLeftDiagonal c r b
    | c < 0 || r < 0 = []
    | otherwise      = ((b !! c) !! r) : rightToLeftDiagonal (c - 1) (r - 1) b

allLeftToRightDiags :: Board -> [[Color]]
allLeftToRightDiags b = leftToRightDiagsCIncr 0 wi b ++ leftToRightDiagsRDecr 0 wi b
    where wi = length (head b) - 1

leftToRightDiagsRDecr :: Column -> Row -> Board -> [[Color]]
leftToRightDiagsRDecr c r b
    | c >= length b || r < 0 = []
    | otherwise              = leftToRightDiagonal c r b : leftToRightDiagsRDecr c (r-1) b

leftToRightDiagsCIncr :: Column -> Row -> Board -> [[Color]]
leftToRightDiagsCIncr c r b
    | c >= length b || r < 0 = []
    | otherwise              = leftToRightDiagonal c r b : leftToRightDiagsCIncr (c+1) r b

leftToRightDiagonal :: Column -> Row -> Board -> [Color]
leftToRightDiagonal c r b
    | c >= length b || r < 0 = []
    | otherwise              = ((b !! c) !! r) : leftToRightDiagonal (c + 1) (r - 1) b
