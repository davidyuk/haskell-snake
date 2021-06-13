module Lib
    ( getField,
      readAction,
      movePoint,
      isSnakeValid,
      randomPoint,
      includes,
      Action(..)
    ) where

import Data.List( elemIndex, find, nub )
import Data.Ix( inRange )
import System.Random( randomIO )
fieldSize = [40, 15]

includes :: Eq a => a -> [a] -> Bool
includes el list = (elemIndex el list) /= Nothing

isSnakeValid :: [(Int, Int)] -> Bool
isSnakeValid snake =
    (find (\(x, y) -> not (inRange (0, fieldSize!!0 - 1) x && inRange (0, fieldSize!!1 - 1) y)) snake) == Nothing
    && nub snake == snake

randomPoint :: IO((Int, Int))
randomPoint = do
    x <- randomIO :: IO Int
    y <- randomIO :: IO Int
    return (mod x (fieldSize!!0), mod y (fieldSize!!1))

getFieldChar :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Char
getFieldChar snake food point =
    if includes point snake then '*' else if point == food then '+' else ' '

getField :: [(Int, Int)] -> (Int, Int) -> String
getField snake food =
    unlines (
        [(replicate (fieldSize!!0 + 2) '_')] ++
        (map
            (\y -> ("|" ++ (map (\x -> getFieldChar snake food (x, y)) [0..(fieldSize!!0 - 1)]) ++ "|"))
            [0..(fieldSize!!1 - 1)])
        ++ [(replicate (fieldSize!!0 + 2) '-')]
    )

data Action = TurnUp | TurnDown | TurnLeft | TurnRight
    deriving (Show, Read, Eq, Ord)

readAction :: IO(Action)
readAction = do
    char <- getChar
    case char of
        'w' -> return TurnUp
        's' -> return TurnDown
        'd' -> return TurnRight
        'a' -> return TurnLeft
        _ -> undefined

movePoint :: (Int, Int) -> Action -> (Int, Int)
movePoint (x, y) action = case action of
    TurnUp -> (x, y - 1)
    TurnDown -> (x, y + 1)
    TurnRight -> (x + 1, y)
    TurnLeft -> (x - 1, y)
