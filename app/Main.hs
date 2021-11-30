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

-- place item, return new board
-- helper function that inserts an item into an array
put :: Int -> a -> [a] -> [a]
put pos newVal list = take pos list ++ newVal : drop (pos+1) list
-- places x or o on the board, return updated board
placeItem :: Int -> Int -> a -> [[a]] -> [[a]]
placeItem x y newVal mat = put y (put x newVal (mat!!y)) mat

-- identifies item occupying a position 
getItem :: Int -> Int -> [[a]] -> a
getItem x y mat = (mat!!y)!!x

-- return multiple single-char strings from array of chars
strFromArr :: String -> [String]
strFromArr = map (\x -> [x])

-- check if player won
    -- if not, continue game
-- check for tie
    -- if not, continue game

-- print board with newly placed piece

-- alternate turns

-- prevent replays


