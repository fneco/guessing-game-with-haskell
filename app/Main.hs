module Main (main) where

import System.Random
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "Guess the number!"

  secretNumber <- randomRIO (1, 100) :: IO Int

  putStrLn ("The secret number is: " ++ show secretNumber)

  compareNumbers secretNumber

compareNumbers :: Int -> IO ()
compareNumbers secretNumber = do
  putStrLn "Please input your guess."

  guess <- getLine

  let maybeGuessedNumber = readMaybe guess :: Maybe Int
  case maybeGuessedNumber of
    Nothing -> compareNumbers secretNumber
    Just guessedNumber -> do
      putStrLn ("You guessed: " ++ guess)

      let ordering = compare guessedNumber secretNumber

      isWin <- case ordering of
        LT -> do
          putStrLn "Too small!"
          return False
        GT -> do
          putStrLn "Too big!"
          return False
        EQ -> return True

      if isWin
        then putStrLn "You win!"
        else compareNumbers secretNumber
