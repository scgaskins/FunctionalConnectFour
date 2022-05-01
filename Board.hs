{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

--newtype BoardState = BS
--{ board :: Array (Column, Row) Pieces}

rows = 6
columns = 7

type Column = Int
type Row = Int
type Coords = (Column, Row)

-- Enumeration for the 3 Colors
data Pieces = Empty | X | O

-- Board creation
data Board = Array (Column, Row) Pieces
