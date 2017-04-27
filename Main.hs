module Main where
import IOInterface (userInterface)

main :: IO()
main = do
  putStr "Enter a relative path to wd: "
  wd <- getLine
  userInterface wd
