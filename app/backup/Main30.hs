module Main (main) where

import System.Random

main :: IO ()
main = do
  putStrLn "Guess the number!"

  secretNumber <- randomRIO (1, 100) :: IO Int

  putStrLn ("The secret number is: " ++ show secretNumber)

  putStrLn "Please input your guess."

  guess <- getLine

  putStrLn ("You guessed: " ++ guess)