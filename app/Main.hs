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

  let guessNumber = read guess :: Int
  let ordering = compare guessNumber secretNumber
  putStrLn
    ( case ordering of
        LT -> "Too small!"
        GT -> "Too big!"
        EQ -> "You win!"
    )
