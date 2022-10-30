module Main where

import System.IO

main :: IO ()
main = do
  handle <- openFile "test.json" ReadMode
  _ <- hGetContents handle
  hClose handle