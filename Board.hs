{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

--rows = 6
--columns = 7

type Column = Int
type Row = Int
type Coords = (Column, Row)

-- Enumeration for the 3 Colors
data Color = Empty | Red | Yellow

-- Board is just a 2 Dimensional List
data Board = Board [[Color]]