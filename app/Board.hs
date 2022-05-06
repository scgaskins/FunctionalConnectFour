{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Board where

import Data.List (group, transpose, intersperse)

type Column = Int
type Row = Int

-- data type color is a Enum for the 3 Colors
data Color = Empty | Red | Yellow
    deriving (Eq, Show)

-- type Board for Connect4 is a 2D List of data type Color
type Board = [[Color]]

-- emptyBoard returns a board full of Empty spaces
emptyBoard :: Int -> Int -> Board
emptyBoard columns rows = replicate columns (replicate rows Empty)

-- formatBoard gives the board new line 
-- characters to split the board by rows
-- to read the board easier for the users
formatBoard :: Board -> String
formatBoard = unlines . map (intersperse ' ' . map formatColor) . transpose

-- formatColor converts colors to a single 
-- Char to make them easier to read
formatColor :: Color -> Char
formatColor Empty  = '*'
formatColor Red    = 'O'
formatColor Yellow = 'X'

-- possibleMoves returns Columns whose topmost 
-- row is still not filled
possibleMoves :: Board -> [Column]
possibleMoves = map fst . filter (\(_,color) -> color == Empty). zip (iterate (+1) 0) . map head

-- makeMove adds the piece to the given column, 
-- returns Nothing if that column is full
makeMove :: Color -> Column -> Board -> Maybe Board
makeMove _ _ []         = Nothing
makeMove color 0 (c:cs) = fmap (: cs) (addToColumn color c)
makeMove color n (c:cs) = fmap (c :) (makeMove color (n-1) cs)

-- changePlayers takes a list of Colors and an Int 
-- and gives back either the first or second color 
-- in the list to represent changing players
changePlayers :: [Color] -> Int -> Color
changePlayers [] _            = Empty
changePlayers [Red, Yellow] 0 = Yellow
changePlayers [Red, Yellow] 1 = Red
changePlayers _ _             = Empty

-- update takes an index and gives back the other 
-- possible player index
update :: Int -> Int
update 0 = 1
update 1 = 0
update _ = error "Invalid index given"

-- addToColumn adds a piece to a column, replacing the last 
-- consecutive Empty space. If there are no Empty 
-- spaces at the top, it returns Nothing
addToColumn :: Color -> [Color] -> Maybe [Color]
addToColumn Empty c                           = Just c
addToColumn color [Empty]                     = Just [color]
addToColumn color (Empty : nextColor : cs)
    | nextColor == Red || nextColor == Yellow = Just (color : nextColor : cs)
    | nextColor == Empty                      = fmap (Empty :) (addToColumn color (nextColor : cs))
addToColumn _ _                               = Nothing

-- checkWin checks if a player of particular color has won
checkWin :: Color -> Board -> Bool
checkWin c b = winByDiagonals c b || winByColumns c b || winByRows c b

-- winByDiagonals checks if there are four in a row of the 
-- color in any diagonals
winByDiagonals :: Color -> Board -> Bool
winByDiagonals color = any (fourInARow color) . allDiagonals

-- winByColumns checks if there are four in a row 
-- of the color in any columns
winByColumns :: Color -> Board -> Bool
winByColumns color = any (fourInARow color) 

-- winByRows checks if there are four in a row of 
-- the color in any rows
winByRows :: Color -> Board -> Bool
winByRows color = winByColumns color . transpose

-- fourInARow checks if there are four pieces of 
-- a given color in the list
fourInARow :: Color -> [Color] -> Bool
fourInARow color colors = longestSequence color colors >= 4

-- longestSequence finds the length of the longest sequence of a 
-- given element in the list
longestSequence :: Eq a => a -> [a] -> Int
longestSequence a = foldr (max . length) 0 . filter (a `elem`) . group

-- allDiagonals generates a list of all diagonal lines in the
-- board
allDiagonals :: Board -> [[Color]]
allDiagonals b = allLeftToRightDiags b ++ allRightToLeftDiags b

-- allRightToLeftDiags generates all diagonal lines in the board
-- that move up and to the left from the starting point
allRightToLeftDiags :: Board -> [[Color]]
allRightToLeftDiags b = rightToLeftDiagsCDecr len wi b ++ rightToLeftDiagsRDecr len wi b
    where (len, wi) = (length b - 1,length (head b) - 1)

-- rightToLeftDiagsCDecr generates all the diagonal 
-- lines that start in the bottom row of the board and 
-- move up and to the left
rightToLeftDiagsCDecr :: Column -> Row -> Board -> [[Color]]
rightToLeftDiagsCDecr c r b
    | c < 0 || r < 0 = []
    | otherwise      = rightToLeftDiagonal c r b : rightToLeftDiagsCDecr (c-1) r b

-- rightToLeftDiagsRDecr generates all the diagonal lines 
-- that start in the last column of the board and move up 
-- and to the left
rightToLeftDiagsRDecr :: Column -> Row -> Board -> [[Color]]
rightToLeftDiagsRDecr c r b
    | c < 0 || r < 0 = []
    | otherwise      = rightToLeftDiagonal c r b : rightToLeftDiagsRDecr c (r-1) b

-- rightToLeftDiagonal generates a diagonal line through 
-- the board moving up and to the left from the starting point
rightToLeftDiagonal :: Column -> Row -> Board -> [Color]
rightToLeftDiagonal c r b
    | c < 0 || r < 0 = []
    | otherwise      = ((b !! c) !! r) : rightToLeftDiagonal (c - 1) (r - 1) b

-- allLeftToRightDiags generates all the diagonal lines in the 
-- board that move up and to the left from their starting point
allLeftToRightDiags :: Board -> [[Color]]
allLeftToRightDiags b = leftToRightDiagsCIncr 0 wi b ++ leftToRightDiagsRDecr 0 wi b
    where wi = length (head b) - 1

-- leftToRightDiagsRDecr generates all the diagonal lines that 
-- start in the first column of the board and move up and to the right
leftToRightDiagsRDecr :: Column -> Row -> Board -> [[Color]]
leftToRightDiagsRDecr c r b
    | c >= length b || r < 0 = []
    | otherwise              = leftToRightDiagonal c r b : leftToRightDiagsRDecr c (r-1) b

-- leftToRightDiagsCIncr generates all the diagonal lines that 
-- start in the bottom row of the board and move up and to the right
leftToRightDiagsCIncr :: Column -> Row -> Board -> [[Color]]
leftToRightDiagsCIncr c r b
    | c >= length b || r < 0 = []
    | otherwise              = leftToRightDiagonal c r b : leftToRightDiagsCIncr (c+1) r b

-- leftToRightDiagonal generates a diagonal line through the board
-- moving up and to the right from the starting point
leftToRightDiagonal :: Column -> Row -> Board -> [Color]
leftToRightDiagonal c r b
    | c >= length b || r < 0 = []
    | otherwise              = ((b !! c) !! r) : leftToRightDiagonal (c + 1) (r - 1) b
