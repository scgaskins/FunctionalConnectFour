{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

-- main: make board and call event loop

-- event loop:

-- draw board
-- is it won?
-- print the win message
-- not won?
-- get a move
-- recursive call to event loop with updated board, next player