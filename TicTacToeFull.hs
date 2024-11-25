module TicTacToeFull where

import Prelude hiding (last)
import Data.List hiding (last)
import Data.Maybe
import Control.Monad
import Text.Read

data Player = O | X
  deriving (Eq, Ord, Show)

type Space = Maybe Player

type Board = [[Space]]

size :: Int
size = 3

empty :: Board
empty = replicate size (replicate size Nothing)

-- \x -> x + 1 = (+1)

isFull :: Board -> Bool
isFull b = all (/= Nothing) (mconcat b)

{--
-------------
|   |   |   |
-------------
|   | X |   |
-------------
|   |   |   |
-------------
--}

sep :: String
sep = replicate (4 * size + 1) '-'

showBoard :: Board -> [String]
showBoard b = sep : (do
  row <- b
  [showRow row, sep])

showRow :: [Space] -> String
showRow row = '|' : (do
  s <- row
  ' ' : showSpace s : " |")

showSpace :: Space -> Char
showSpace Nothing  = ' '
showSpace (Just X) = 'X'
showSpace (Just O) = 'O'

printBoard :: Board -> IO ()
printBoard b = mapM_ putStrLn (showBoard b)

{--
printBoard :: Board -> IO ()
printBoard b = go (showBoard b)
  where go :: [String] -> IO ()
        go []     = return ()
        go (x:xs) = do
          putStrLn x
          go xs
--}

index :: Board -> Int -> Space
index b i = mconcat b !! i 

isValid :: Board -> Int -> Bool
isValid b i = i >= 0 && i < size^2 && index b i == Nothing

move :: Board -> Int -> Player -> Maybe Board
move b i p | isValid b i = Just $ chop size (xs ++ [Just p] ++ ys) 
           | otherwise   = Nothing
    where (xs, _:ys) = splitAt i (mconcat b)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

wins :: Board -> Player -> Bool
wins b p = any lines (rows ++ cols ++ dias)
  where lines = all (==Just p)
        rows  = b 
        cols  = transpose b
        dias  = [diags b, diags (reverse b)]

diags :: Board -> [Space]
diags b = [ b !! i !! i | i <- [0..size-1] ]

won :: Board -> Bool
won b = wins b O || wins b X

next :: Player -> Player
next O = X
next X = O

last :: Player -> Player
last = next

getIndex :: String -> IO Int
getIndex prompt = do
  putStrLn prompt
  s <- getLine
  case readMaybe s of
    Just i -> return i
    Nothing -> do
      putStrLn "Error: not a number."
      getIndex prompt

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

runBoard :: Board -> Player -> IO ()
runBoard b p | wins b (last p) = putStrLn ("Player " ++ show (last p) ++ " wins!")
             | isFull b = putStrLn "It is a draw!"
             | p == X = do
                 putStrLn "Player X is thinking..."
                 run (bestmove b p) (next p)
             | otherwise = do
                 i <- getIndex ("It's player " ++ show p ++ "'s turn. Enter your move:")
                 case move b i p of
                   Just b' -> run b' (next p)
                   Nothing -> do
                     putStrLn "Error: invalid move."
                     runBoard b p
                    
run :: Board -> Player -> IO ()
run b p = do
  cls
  goto (1, 1)
  printBoard b
  runBoard b p

tictactoe :: IO ()
tictactoe = run empty O

data Tree a = Node a [Tree a]
  deriving Show

leaf :: Tree Int
leaf = Node 42 []

treeA :: Tree Char
treeA = Node 'a' [Node 'b' [], Node 'c' [Node 'd' []]]

treeB :: Tree Int
treeB = Node 10 [ Node i [] | i <- [0..9] ]

-- If you are done:
-- All the possible moves given the current board and the player
moves :: Board -> Player -> [Board]
moves b p | won b || isFull b = []
          | otherwise = catMaybes [ move b i p | i <- [0..size^2-1] ]

-- The entire game tree given the current board and the player
gameTree :: Board -> Player -> Tree Board
gameTree b p = Node b [ gameTree b' (next p) | b' <- moves b p]


-- This operation "prunes" the tree to only contain nodes up to certain depth
atDepth :: Tree a -> Int -> Tree a
atDepth (Node x ts) n | n > 0 = Node x [ atDepth t (n - 1) | t <- ts ]
                      | otherwise = Node x []

depth :: Int
depth = 9

newtype Winner = Winner (Maybe Player)
  deriving (Eq, Show)

instance Ord Winner where
  Winner (Just O) <= Winner Nothing  = True
  Winner Nothing  <= Winner (Just O) = False  
  Winner x <= Winner y = x <= y

-- minimum :: Ord a => [a] -> a
-- maximum :: Ord a => [a] -> a

alphabeta :: Tree Board -> Player -> Tree (Winner, Board)
alphabeta (Node b []) p | wins b O = Node (Winner (Just O), b) []
                        | wins b X = Node (Winner (Just X), b) []
                        | otherwise = Node (Winner Nothing, b) []
alphabeta (Node b ts) p = Node (w, b) ws
  where w  :: Winner
        w | p == O    = minimum ps
          | otherwise = maximum ps
        ps = [ w | Node (w, _) _ <- ws ]
        ws :: [Tree (Winner, Board)]
        ws = [ alphabeta t (next p) | t <- ts ]

bestmove :: Board -> Player -> Board
bestmove b p = head [ b' | Node (w', b') _ <- ts, w == w' ]
  where
    gt = gameTree b p `atDepth` depth
    Node (w, _) ts = alphabeta gt p
