module Main where
import           Data.Tree as Tree
import           Data.List.Split
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Text.Read
import           Board
import           Data.Maybe
import           Data.Char

toInt:: String -> Int
toInt string = read string

parsePosition :: String -> Position
parsePosition stringPosition = parse (words stringPosition)

parse :: [String] -> Position
parse [row, col]
    | x >= 1 && x <= 19 && y >= 1 && y <= 19 = Position x y
    | otherwise = Position 0 0
    where
        x = toInt row
        y = toInt col

gameLoop:: Board->IO ()
gameLoop board = do
    if (gameIsOver newBoard X)
        then do
            putStrLn "Przegrałeś z komputerem ;)"
        else do
            putStrLn $ showBoard board
            putStrLn "Jesteś kółkiem, wpisz pozycje np. (1 2): "
            position <- getLine
            let parsedPosition = parsePosition position
            if(free parsedPosition board )
                    then do
                        let newBoard = addPositionToBoard board parsedPosition O
                        if (gameIsOver newBoard O)
                        then do
                            putStrLn $ showBoard newBoard
                            putStrLn "Wygrałeś!"
                        else do
                            let boardAfterMinmax = miniMaxBoard newBoard X
                            gameLoop boardAfterMinmax
             else do
                putStrLn "Zła pozycja!"
                gameLoop board
                putStr("")

main :: IO ()
main = do
    let empty = newBoard
    gameLoop empty
    putStr("")