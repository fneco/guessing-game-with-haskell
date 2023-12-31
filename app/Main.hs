module Main (main) where

import Control.Monad (when)
import System.Random (randomRIO)
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "Guess the number!"
  secretNumber <- randomRIO (1, 100)

  compareNumbers secretNumber

compareNumbers :: Int -> IO ()
compareNumbers secretNumber = do
  putStrLn "Please input your guess."
  guess <- getLine

  let callMyself _ = compareNumbers secretNumber
  case readMaybe guess of
    Nothing -> callMyself ()
    Just guessedNumber -> do
      putStrLn ("You guessed: " ++ guess)

      let ordering = compare guessedNumber secretNumber
      let (message, shouldContinue) = case ordering of
            LT -> ("Too small!", True)
            GT -> ("Too big!", True)
            EQ -> ("You win!", False)

      putStrLn message
      when shouldContinue $ callMyself ()