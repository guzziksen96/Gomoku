module Board where
--import           Data.Tree as Tree
import           Data.List.Split
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Text.Read
import           Data.Foldable (for_)
import           Data.Maybe
import           Data.Char
import           Data.Ord

class ToChar a where
    toChar :: a -> Char
instance ToChar Char where
    toChar = id
instance ToChar Symbol where
    toChar = head . show

data Position = Position {getX::Int, getY:: Int}
    deriving (Show, Eq, Ord, Read)
data Symbol = O | X | Blank deriving (Eq, Read)
data Board = Board (Map Position Symbol) deriving (Eq)
data Tree a = Empty | Node a [Tree a] deriving (Eq,Show)
instance Show Symbol where
    show Blank = " "
    show O     = "O"
    show X     = "X"

instance Show Board where
    show = showBoard

rows :: Board -> [[Symbol]]
rows board = chunksOf 19 $ Map.elems (getBoard board)

cols :: Board -> [[Symbol]]
cols b = transpose (rows b)

showBoard :: Board -> String
showBoard b = unlines $  map (++  "\n") eachRow
  where eachRow = map show (rows b)

showPositions :: String
showPositions = unlines $ map (++ "\n") eachRow
  where eachRow = map show (chunksOf 19 positions)

showMaybe :: (ToChar a) => Maybe a -> Char
showMaybe Nothing = ' '
showMaybe (Just a) = toChar a

getBoard :: Board -> Map Position Symbol
getBoard (Board board) = board

newBoard :: Board
newBoard = Board $ Map.fromAscList [(p,Blank) | p <- positions]
--fromAscList :: [(Key, a)] -> IntMap a

positions :: [Position]
positions = [Position x y | x <- [1..19], y <- [1..19]]

rowPositions :: Position -> [Position]
rowPositions (Position px py) = [Position x py | x <- [px+1, px+2, px-1,px-2,px], x>=1, x<=19]

colPositions :: Position -> [Position]
colPositions (Position px py)= [Position px y | y <- [py+1, py+2, py-1,py-2,py], y>= 1, y<=19]

diagPositions1 :: Position -> [Position]
diagPositions1 (Position px py) = [Position x y | (x,y) <- [(px+1, py+1), (px+2,px+2), (px-1, py-1), (px-2, py-2), (px,py)], x>=1, x<=19, y>= 1, y<=19]

diagPositions2 :: Position -> [Position]
diagPositions2 (Position px py) = [Position x y | (x,y) <- [(px+1, py-1), (px+2,px-2), (px-1, py+1), (px-2, py+2), (px,py)], x>=1, x<=19, y>= 1, y<=19]

(!) :: Position -> Board -> Maybe Symbol
(!) pos board = Map.lookup pos (getBoard board)
--lookup :: Ord k => k -> Map k a -> Maybe a

getSymbol :: Position -> Board -> Maybe Symbol
getSymbol position board = Map.lookup position (getBoard board)

free :: Position -> Board -> Bool
free pos board = (pos ! board) == Just Blank

hasNeighbors :: Position -> [Position] -> Bool
hasNeighbors (Position x y) listOfPositions =
    Prelude.or (Prelude.map (\(tx, ty) -> ((Position (x+tx) (y+ty)) `elem` listOfPositions)) [(x,y) | x<-[-1..1], y<-[-1..1], not(x==0 && y==0)])

possibleMoves :: Board -> [Position]
possibleMoves (Board board) = [Position x y | x <- [1..19], y <- [1..19], free (Position x y) (Board board), hasNeighbors (Position x y) (Map.keys board)]

compare (Position x1 y1) (Position x2 y2) | x1==x2 && y1==y2 = EQ | x1>x2 && y1>y2 = GT | x1<x2 && y1<y2= LT
	| x1>x2 && y1<y2= LT | x1<x2 && y1>y2= GT

getNextSymbol :: Symbol -> Symbol
getNextSymbol symbol
    | symbol == O = X
    | symbol == X = O

addPositionToBoard :: Board -> Position -> Symbol -> Board
addPositionToBoard (Board board) position symbol = Board $ (Map.insert position symbol board)

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

win :: Board -> Position -> Bool
win board (Position px py)
    | allTheSame (map (\x -> getSymbol x board) (rowPositions (Position px py))) && getSymbol (Position px py) board /= Just Blank && length (rowPositions (Position px py)) == 5 = True
    | allTheSame (map (\x -> getSymbol x board) (colPositions (Position px py))) && getSymbol (Position px py) board /= Just Blank && length (colPositions (Position px py)) == 5 = True
    | allTheSame (map (\x -> getSymbol x board) (diagPositions1 (Position px py))) && getSymbol (Position px py) board /= Just Blank && length (diagPositions1 (Position px py)) == 5 = True
    | allTheSame (map (\x -> getSymbol x board) (diagPositions2 (Position px py))) && getSymbol (Position px py) board /= Just Blank && length (diagPositions2 (Position px py)) == 5 = True
    | otherwise = False

-- do szybkich teststów
board = addPositionToBoard newBoard (Position 1 1) X
board2 = addPositionToBoard board (Position 2 2) X
board3 = addPositionToBoard board2 (Position 3 3) X
board4 = addPositionToBoard board3 (Position 4 4) X
board5 = addPositionToBoard board4 (Position 5 5) X

gameIsOver :: Board -> Symbol -> Bool
gameIsOver board symbol
    | (symbol /= Blank) && or ((map (\x -> win board x) positions)) = True
    | otherwise = False

amountInRow :: Position -> Symbol -> Board -> Int -> Int
amountInRow position player board number
				| symbol == Blank = number
				| player /= symbol = number
				where
                    symbol = fromJust (getSymbol position board)
amountInRow position player board number
                | getSymbol nextPosition board == Nothing = number
                | otherwise = amountInRow nextPosition player board (number + 1)
                where
                    (Position a b) = position
                    nextPosition = Position a (b+1)

amountInCol :: Position -> Symbol -> Board -> Int -> Int
amountInCol position player board number
				| symbol == Blank = number
				| player /= fromJust (getSymbol position board) = number
				where
                    symbol = fromJust (getSymbol position board)
amountInCol position player board number
                | getSymbol nextPosition board == Nothing = number
                | otherwise = amountInCol nextPosition player board (number + 1)
                where
                    (Position a b) = position
                    nextPosition= Position (a+1) b

amountInDiag :: Position -> Symbol -> Board -> Int -> Int
amountInDiag position player board number
				| symbol == Blank = number
				| player /= fromJust (getSymbol position board) = number
				where
                    symbol = fromJust (getSymbol position board)
amountInDiag position player board number
                | getSymbol nextPosition board == Nothing = number
                | otherwise = amountInDiag nextPosition player board (number + 1)
                where
                    (Position a b) = position
                    nextPosition= Position (a+1) (b+1)

pointRating :: Int -> Int
pointRating a
				| a == 1 = 5
				| a == 2 = 20
				| a == 3 = 40
				| a == 4 = 100
				| a == 5 = 1000
				| otherwise	= 0

evaluateBoard :: Board -> Symbol -> Int
evaluateBoard board symbol = sum allPoints
				where
				    --0 zlicza ilosc elementow
					pointsInRow =  map (\x -> sum $ map (\y -> pointRating $ amountInRow (Position x y) symbol board 0) [1..19]) [1..19]
					pointsInColumn =  map (\x -> sum $ map (\y -> pointRating $ amountInCol (Position x y)  symbol board 0) [1..19]) [1..19]
					pointsInDiagonal =  map (\x -> sum $ map (\y -> pointRating $ amountInDiag (Position x y) symbol board 0) [1..19]) [1..19]
					allPoints = pointsInRow ++ pointsInColumn ++ pointsInDiagonal

generateTree :: Board -> Int -> Symbol -> Tree Board
generateTree board depth symbol
				| depth == 0 = Node board []
				| otherwise = Node board boards
				where
					boards = map (\x -> generateTree x (depth - 1) (getNextSymbol symbol))  (generateMoves board symbol)

generateMoves :: Board -> Symbol -> [Board]
generateMoves board symbol = boards
                    where
                        boards = map (\x -> addPositionToBoard board x symbol) (possibleMoves board)

minimax :: Symbol -> Int -> Bool -> Tree Board -> Int
minimax symbol depth bool (Node a children)
    | depth == 0 = evaluateBoard a symbol
    | bool == True = maximum (map (minimax symbol (depth-1) False) children)
    | bool == False = minimum (map (minimax symbol (depth-1) True) children)

miniMaxBoard:: Board -> Symbol -> Board
miniMaxBoard board symbol
        | length movesPossibleNow == 0 = addPositionToBoard board (Position 9 9) symbol
        | otherwise = bestBoard
        where
            movesPossibleNow = possibleMoves board
            trees = map (\move -> (generateTree (addPositionToBoard board move symbol) maxDepth symbol)) movesPossibleNow --dla kazdego mozliwego ruchu plansza
            maxDepth = 1 --maksymalna glebokosc dla minmaxa
            minMaxMoves = map (minimax symbol maxDepth True) trees --minmax dla kazdego mozliwego ruchu
            index = fromJust (elemIndex (maximum minMaxMoves) minMaxMoves) --indeks gdzie minmax jest maksymalny
            bestBoard = addPositionToBoard board (movesPossibleNow !! index) symbol --plansza z ruchem gdzie minmax jest najwiekszy
            -- !! Bierze z listy element spod indeksu