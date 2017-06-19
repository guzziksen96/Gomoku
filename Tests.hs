module Tests where
import           Test.HUnit
import           Data.Tree as Tree
import           Board

checkIfWin :: Test
checkIfWin = TestCase $ do
     let board = addPositionToBoard newBoard (Position 1 1) X
     let board2 = addPositionToBoard board (Position 1 2) X
     let board3 = addPositionToBoard board2 (Position 1 3) X
     let board4 = addPositionToBoard board3 (Position 1 4) X
     let board5 = addPositionToBoard board4 (Position 1 5) X
     let expectedValue = True
     let generatedValue =  win board5 (Position 1 3)
     assertEqual "?" expectedValue generatedValue

checkEvaluationBoard :: Test
checkEvaluationBoard = TestCase $ do
    let expectedValue = 95
    let generatedValue = evaluateBoard board3 X
    assertEqual "?" expectedValue generatedValue

checkEvaluationBoardForO :: Test
checkEvaluationBoardForO = TestCase $ do
    let expectedValue = 0
    let generatedValue = evaluateBoard board3 O
    assertEqual "?" expectedValue generatedValue

allTests = TestList [checkIfWin, checkEvaluationBoard, checkEvaluationBoardForO]

--main :: IO()
--main = runTestTT allTests