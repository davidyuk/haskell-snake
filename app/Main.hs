module Main where

import Lib
import System.IO( stdin, hReady, hSetEcho, hSetBuffering, BufferMode( NoBuffering ) )
import System.Timeout( timeout )
import Data.Maybe( fromMaybe )

main :: IO()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    snakeHead <- randomPoint
    food <- randomPoint
    putStr "\ESC[2J"
    putStrLn (getField [snakeHead] food)
    action <- readAction
    loop [snakeHead] action food
    putStrLn "Game over"

loop :: [(Int, Int)] -> Action -> (Int, Int) -> IO()
loop snake action food = do
    putStr "\ESC[2J"
    let time = 150000 + round(250000 * maximum [fromIntegral (11 - length snake) / 10, 0])
    nextActionMaybe <- timeout time readAction
    let nextActionIndirect = fromMaybe action nextActionMaybe
    let nextAction = if length snake /= 1 && movePoint (snake!!0) nextActionIndirect == snake!!1 then action else nextActionIndirect

    let nextSnakeHead = movePoint (snake!!0) nextAction
    let ate = food == nextSnakeHead
    let nextSnake = nextSnakeHead : if ate then snake else init snake
    nextFoodRandom <- randomPoint
    let nextFood = if ate then nextFoodRandom else food

    if isSnakeValid nextSnake
        then do
            putStrLn ("Snake length: " ++ (show (length snake)))
            putStrLn (getField nextSnake nextFood)
            loop nextSnake nextAction nextFood
        else
            return ()
