module Lib
  ( compareNumbers,
  )
where

import Text.Read (readMaybe)

compareNumbers :: Int -> IO ()
compareNumbers secretNumber = do
  putStrLn "Please input your guess."

  guess <- getLine

  let maybeGuessedNumber = readMaybe guess :: Maybe Int
  let guessedNumber = case maybeGuessedNumber of
        Just parsedInput -> parsedInput
        Nothing -> error "Please type a number!"

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
