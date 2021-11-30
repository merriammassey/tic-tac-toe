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


-- turns user input into a Move, checking to see if it's a valid move
makeMove :: String -> (Move, Bool)
makeMove str
    -- if the user enters more than 2 characters, it's an invalid move
    | length str /= 2                             = invalidMove -- Other than 2 chars
    | (elem l ['A'..'Z']) && (elem n ['0'..'9'])  = ( ((ord l)-65, (ord n)-48), True )
    | otherwise                                   = invalidMove -- Invalid Move
  where
    l = str!!0 -- Char letter
    n = str!!1 -- Char number
    invalidMove = ((0,0), False)

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

-- check for a winner 
checkForWin :: Board -> Move -> Bool
checkForWin b m = vert || horiz || diagUpperLeft || diagUpperRight
  where
    dUL             = diagonalFromLeft b -- The upper left daigonal array
    dUR             = diagonalFromRight b -- The upper right diaganal array
    -- check the row, column, and diagonals from last move to see if they all contain same player
    vert            = checkForSame $ b !! (snd m)
    horiz           = checkForSame $ map (!! (fst m)) b
    diagUpperLeft   = (not $ all (== ' ') dUL) && (checkForSame dUL)
    diagUpperRight  = (not $ all (== ' ') dUR) && (checkForSame dUR)

-- check to see if board is full
fullBoard :: Board -> Bool
fullBoard = not . foldr1 (||) . map (any(==' '))

-- return updated board if move is valid
updatedBoard :: Board -> Move -> Char -> (Board, Bool)
updatedBoard b m player
    -- position doesn't exist
    | x < 0 || y < 0 || x >= w || y >= h  = (b, False) -- Out of bounds
    -- position not empty
    | getItem x y b /= ' '                  = (b, False) 
    | otherwise                           = (placeItem x y player b, True)
  where
    x = fst m  -- x coordinate for the move
    y = snd m  -- y coordinate for the move
    w = length $ head b  -- width of board
    h = length b -- height of board

-- alternate turns
turn :: Char -> Char
turn current = if current == 'x' then 'o' else 'x'

-- if the user chooses to play again, restart the game 
restart :: IO()
restart = do
  putStrLn "Would you like to play again? (Y/N)"
  playAgain <- getLine
  if playAgain == "Y" || playAgain == "y" then do
    putStrLn $ take 20 $ repeat '\n'
    tictactoe emptyBoard 'x'
  else if playAgain == "N" || playAgain == "n" then
    return ()
  else do
    putStrLn "Invalid input. Please enter 'Y' or 'N'"
    restart

