-- module Main where

-- import Lib

-- main :: IO ()
-- main = someFunc

-- the board = a list of strings
type Board = [[Char]]

-- a move = a pair of integers
type Move = (Int, Int)

-- the board, full of empty spaces to begin
emptyBoard = ["   ", "   ", "   "]


-- Helper Functions

-- make a board with labeled rows (numbers) and columns (letters)
makeBoard :: Board -> String
makeBoard board =
  letters ++ "\n\n" ++ (intercalate rowBorder $ numbers $ map columnBorder board)
  where
    width = length $ head board
    -- add border to columns
    columnBorder = intercalate " | " . map (\x -> [x])
    -- add border to rows
    rowBorder = "\n   " ++ (tail $ init $ intercalate "+" $ take width $ repeat "---") ++ "\n"
    -- add ABC to columns
    letters = "   " ++ (intercalate "   " $ strFromArr $ take width ['A'..])
    -- add 123 to rows
    numbers s = [(show n) ++ "  " ++ x | n <- [0..(length s)-1], x <- [s!!n]]


-- check if position is open 

-- place piece in position chosen

-- check if player won
    -- if not, continue game
-- check for tie
    -- if not, continue game

-- print board with newly placed piece

-- alternate turns

-- prevent replays


