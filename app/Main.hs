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

  let maybeGuessNumber = readMaybe guess :: Maybe Int
  let guessNumber = case maybeGuessNumber of
        Just parsedInput -> parsedInput
        Nothing -> error "Please type a number!"

  putStrLn ("You guessed: " ++ guess)

  let ordering = compare guessNumber secretNumber
  putStrLn
    ( case ordering of
        LT -> "Too small!"
        GT -> "Too big!"
        EQ -> "You win!"
    )
