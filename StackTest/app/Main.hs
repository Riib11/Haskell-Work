module Main where

import Lib

main :: IO ()
main = do
  input <- getLine
  putStrLn $ "\n" ++ input
  main

