module Main (main) where

import System.Random
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "Guess the number!"

  secretNumber <- randomRIO (1, 100) :: IO Int

  putStrLn ("The secret number is: " ++ show secretNumber)

  putStrLn "Please input your guess."

  guess <- getLine

  putStrLn ("You guessed: " ++ guess)

  let maybeGuessedNumber = readMaybe guess
  let guessedNumber = case maybeGuessedNumber of
        Just parsedInput -> parsedInput
        Nothing -> error "Please type a number!"
  let ordering = compare guessedNumber secretNumber
  let message = case ordering of
        LT -> "Too small!"
        GT -> "Too big!"
        EQ -> "You win!"

  putStrLn message
