module Main (main) where

import Control.Monad (when)
import System.Random
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "Guess the number!"

  secretNumber <- randomRIO (1, 100) :: IO Int

  compareNumbers secretNumber

compareNumbers :: Int -> IO ()
compareNumbers secretNumber = do
  putStrLn "Please input your guess."

  guess <- getLine

  case readMaybe guess :: Maybe Int of
    Nothing -> compareNumbers secretNumber
    Just guessedNumber -> do
      putStrLn ("You guessed: " ++ guess)

      let ordering = compare guessedNumber secretNumber

      let (message, shouldContinue) = case ordering of
            LT -> ("Too small!", False)
            GT -> ("Too big!", False)
            EQ -> ("You win!", True)

      putStrLn message
      when shouldContinue $ compareNumbers secretNumber
