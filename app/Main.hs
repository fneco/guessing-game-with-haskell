module Main (main) where

main :: IO ()
main = do
  putStrLn "Guess the number!"

  putStrLn "Please input your guess."

  guess <- getLine

  putStrLn ("You guessed: " ++ guess)
