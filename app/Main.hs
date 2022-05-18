module Main where


import System.IO ()

import CLI


main :: IO ()
main = do
  parseArgs >>= print
