module Main (main) where

import Lib (compareNumbers)
import System.Random

main :: IO ()
main = do
  putStrLn "Guess the number!"

  secretNumber <- randomRIO (1, 100) :: IO Int

  putStrLn ("The secret number is: " ++ show secretNumber)

  compareNumbers secretNumber
