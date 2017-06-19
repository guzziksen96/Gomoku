# Gomoku

Introduction

Artificial Intelligence in Gomoku Game using Minimax algorithm implemented in haskell.

How to play

So as to play you need to have ghc haskell compiler installed. Call "main" function without any argument from main.hs in terminal. Then just follow commands.
    
Modules

    Main.hs is the main module. It contains main fuction, which loops the game
    
    Board.hs has the basic gomoku typeclasses and functions. Implementation of an board and position. Functions that are able to put position on board, evaluate board, check if game is over (there are five the same symbol in one line), minimax algoritm, generate a tree ... 
    
    Tests.hs is a module which contain some unit tests. Run 'runTestTT allTests' to run all tests
