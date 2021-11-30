main :: IO ()
main = do
putStrLn $ take 10 $ repeat '\n'
  putStrLn "\nWelcome to Tic Tac Toe\n"
  putStrLn "Where columns are represented by letters,"
  putStrLn "and rows are represented by numbers.\n"
  putStrLn "  A   B   C  "
  putStrLn "0   |   |   "
  putStrLn "  --------- "
  putStrLn "1   | X |   "
  putStrLn "  --------- "
  putStrLn "2   |   |   \n"
  putStrLn "To make a move, type the letter and number"
  putStrLn "that represent the cell you wish to move to.\n"
  putStrLn "For example, B1 will place your mark" 
  putStrLn "in the middle cell as shown above.\n"
  putStrLn "Type 'exit' at any time to quit.\n"
  putStrLn "When you are ready to begin, press 'Enter'.\n"

  -- get user input to determine if the user wants to exit
  input <- getLine
  if input == "exit" then
    return ()
    -- otherwise, begin with player x
    else do
        putStrLn $ take 20 $ repeat '\n'
        tictactoe emptyBoard 'x'
  
tictactoe :: Board -> Char -> IO()
tictactoe board playerChar = do
    putStrLn $ "\n" ++ (makeBoard board) ++ "\n"
    putStrLn $ "Player " ++ [playerChar] ++ ", please enter your move: "
    line <- getLine
    -- make space for the board
    putStrLn $ take 20 $ repeat '\n'
    -- quit based on user input
    if "exit" == line then
      return ()
    else do
      -- get move
      let move = makeMove line
      -- if the move is valid
      if snd move then do
        -- insert playerChar and update the board
        let newBoardTuple = updatedBoard board (fst move) playerChar
        if snd newBoardTuple then do
          -- updated board
          let newBoard = fst newBoardTuple
          -- check for a win
          if (checkForWin newBoard (fst move)) then do
            putStrLn $ makeBoard newBoard
            putStrLn $ "Player " ++ [playerChar] ++ " is the winner!"
            restart
          -- check for a tie
          else if fullBoard newBoard then do
            putStrLn $ makeBoard newBoard
            putStrLn $ "It's a tie!"
            restart
          else
            -- if neither win nor tie, continue game
            tictactoe newBoard (turn playerChar) 
        -- if the move was invalid because it does not exist or is taken
        else do
          putStrLn "Invalid move!"
          putStrLn "Please try again."
          tictactoe board playerChar 
      -- if the user input is invalid
      else do
        putStrLn "Invalid move.\nPlease try again."
        tictactoe board playerChar 

-- the board = a list of strings
type Board = [[Char]]

-- a move = a pair of integers
type Move = (Int, Int)

-- the board, full of empty spaces to begin
emptyBoard = ["   ", "   ", "   "]

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
    | length str /= 2 = invalidMove -- Other than 2 chars
    | (elem l ['A'..'Z']) && (elem n ['0'..'9']) = ( ((ord l)-65, (ord n)-48), True )
    | otherwise = invalidMove -- Invalid Move
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
    dUL = diagonalFromLeft b -- The upper left daigonal array
    dUR = diagonalFromRight b -- The upper right diaganal array
    -- check the row, column, and diagonals from last move to see if they all contain same player
    vert = checkForSame $ b !! (snd m)
    horiz = checkForSame $ map (!! (fst m)) b
    diagUpperLeft = (not $ all (== ' ') dUL) && (checkForSame dUL)
    diagUpperRight = (not $ all (== ' ') dUR) && (checkForSame dUR)

-- checks to see if all items in list are the same (win)
checkForSame :: Eq a => [a] -> Bool
checkForSame (x:xs) = all (==x) xs
-- checks right to left diagonal
diagonalFromRight :: [[a]] -> [a]
diagonalFromRight xs = [(xs!!n)!!n | n <- [0..(length xs) -1]]
-- checks left to right diagonal
diagonalFromLeft :: [[a]] -> [a]
diagonalFromLeft xs = [(xs!!n)!!(len - n -1) | n <- [0..len-1]]
  where len = length xs

-- check to see if board is full
fullBoard :: Board -> Bool
fullBoard = not . foldr1 (||) . map (any(==' '))

-- return updated board if move is valid
updatedBoard :: Board -> Move -> Char -> (Board, Bool)
updatedBoard b m player
    -- position doesn't exist
    | x < 0 || y < 0 || x >= w || y >= h = (b, False) -- Out of bounds
    -- position not empty
    | getItem x y b /= ' ' = (b, False) 
    | otherwise = (placeItem x y player b, True)
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

