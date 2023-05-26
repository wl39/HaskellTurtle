module Main where

import System.Environment
import Data.Either
import Data.Typeable
import Data.Char


import Parsing
import Turtle
import Lang

main = do
    args <- getArgs -- get the argument
    text <- readFile (args!!0) -- args!!0 == args[0]
    let defsFromFile = toFunc text -- make a list of function definitions
    putStrLn("Please enter how many decimal places comparisons should round to:")
    input <- getLine
    putStrLn("Please enter a frames per second of precision: ")
    inputFps <- getLine
    let stringPercision = "" ++ input
    let stringFps = "" ++ inputFps
    if (all isDigit stringPercision == False)
    then do
      putStrLn("Integers only")
      main
    else if (all isDigit stringFps == False)
            then do
              putStrLn("Integers only")
              main
            else do
                let precision = [("!", Fn [] (Precision (read stringPercision::Int)))]
                case (interp (precision++defsFromFile) [] (Call ("main") [])) of
                        Left commands -> runTurtle commands (read stringFps)
                        Right errorMsg -> putStrLn(errorMsg)
