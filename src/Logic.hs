{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

{-|
Core game logic
-}

module Logic where

import Data.Matrix
import Data.Maybe
import qualified Data.Vector as V

import System.Random

-- * Core types

-- |A board square is either empty or has a tile with an integer
type Square = Maybe Integer

-- |A board is a matrix of board squares.
-- Square (1,1) is the top left-hand square.
type Board = Matrix Square

-- |Tiles can be slided in four directions
data Direction = Up | Down | LeftD | RightD
    deriving (Eq, Show)

-- * Constants

-- |Size of the square board (4x4 in the original 2048)
boardSize :: Int
boardSize = 4

-- |Probability to get a new tile with a 4 (v.s. a 2)
probabilityTileWith4 :: Double
probabilityTileWith4 = 0.1

-- * Key functions

-- |Empty board of size boardSize x boardSize
emptyBoard :: Board
emptyBoard = fromList boardSize boardSize $ repeat Nothing

-- |Add new random tiles on empty positions on the board, if possible
addNewRandomTiles :: RandomGen g => g -> Board -> Maybe (Board, g)
addNewRandomTiles gen board =
    let empty_squares = emptySquares board in
        case (length empty_squares >= 2) of
            True -> Just $ addNewRandomTiles' gen board empty_squares
            False -> Nothing

addNewRandomTiles' :: RandomGen g => g -> Board -> [(Int, Int)] -> (Board, g)
addNewRandomTiles' gen board empty_squares =
    let (new_tiles_pos, gen') = getNewRandomTilePositions gen 2 empty_squares
        (new_tiles, gen'') = getNewRandomTiles gen' 2 in
            (foldl f board $ zip new_tiles_pos new_tiles, gen'')
    where f b ((i, j), sq) = setElem sq (i, j) b

getNewRandomTiles :: RandomGen g => g -> Int -> ([Square], g)
getNewRandomTiles gen 0 = ([], gen)
getNewRandomTiles gen n =
    let (l, gen') = getNewRandomTiles gen (n-1)
        (x, gen'') = getNewRandomTile gen' in
            (x:l, gen'')

getNewRandomTile :: RandomGen g => g -> (Square, g)
getNewRandomTile gen =
    let (x :: Double, gen') = random gen in
        (if x < probabilityTileWith4 then Just 4 else Just 2, gen')

getNewRandomTilePositions :: RandomGen g => g -> Int -> [(Int, Int)] -> ([(Int, Int)], g)
getNewRandomTilePositions gen 0 _l = ([], gen)
getNewRandomTilePositions gen n l =
    let (xs, gen') = getNewRandomTilePositions gen (n-1) l
        (x, gen'') = randomL gen' l in
            (x:xs, gen'')

randomL :: RandomGen g => g -> [a] -> (a, g)
randomL gen l =
    let (idx, gen') = randomR (0, length l - 1) gen in
        (l !! idx, gen')

-- |Slide tiles based on user input
slideTiles :: Direction -> Board -> Board
slideTiles Up b = foldl1 (<|>) $ map (colVector . slideUpColumn . ((flip getCol) b)) [1..boardSize]
slideTiles LeftD b = transpose $ slideTiles Up $ transpose b
slideTiles RightD b = transpose $ slideTiles Down $ transpose b
slideTiles Down b = flipBoard $ slideTiles Up $ flipBoard b

flipBoard b = foldl1 (<->) $ map (rowVector . ((flip getRow) b)) $ reverse [1..boardSize]

slideUpColumn :: V.Vector Square -> V.Vector Square
slideUpColumn v =
    if (V.length v < 2) then v else
        case ((v V.! 0), (v V.! 1)) of
            (Nothing, _) -> V.snoc (slideUpColumn (V.tail v)) Nothing
            (Just x, Just y) -> if x == y
                then V.snoc (V.cons (Just $ 2*x) (V.drop 2 v)) Nothing
                else V.cons (v V.! 0) $ slideUpColumn (V.tail v)
            (Just x, Nothing) -> V.snoc (slideUpColumn (V.cons (v V.! 0) (V.drop 2 v))) Nothing


getDirection :: IO Direction
getDirection = do 
    l <- getLine
    case l of
        "up" -> return Up
        "down" -> return Down
        "left" -> return LeftD
        "right" -> return RightD
        _ -> getDirection

emptySquares :: Board -> [(Int, Int)]
emptySquares board = [(i,j) | i <- [1..boardSize], j <- [1..boardSize], isNothing $ getElem i j board]

step g b = do
    putStrLn $ show b
    d <- getDirection
    let b' = slideTiles d b
    case addNewRandomTiles g b' of
        Nothing -> return ()
        Just (b'', g') -> step g' b''

play :: IO ()
play = do
    let Just (board, gen) = addNewRandomTiles (mkStdGen 0) emptyBoard in do
            step gen board
