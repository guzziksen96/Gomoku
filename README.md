# Gomoku

Introduction

Artificial Intelligence in Gomoku Game using Minimax algorithm implemented in haskell.

How to play

So as to play you need to have ghc haskell compiler installed. Call "main" function without any argument from main.hs in terminal. Then just follow commands.

Task list

    Create Board.hs module, which implements basic game mechanisms.
    Create 'main' function in main.hs, which makes you able to play Gomoku in command line interface.
    Create Tests.hs module.
    
Modules

    main.hs is the main module. It contains main fuction, which loops the game
    Board.hs has the basic gomoku typeclasses and functions. Implementation of an board and position. Functions that are able to put position on board and evaluate whether the game is won (there are five the same symbol in one line). 
    Tests.hs is an module which contain some unit tests. Run 'runTestTT allTests' to run all tests.
