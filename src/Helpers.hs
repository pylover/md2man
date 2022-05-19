module Helpers
  ( trim
  , upper
  ) where


import Data.Char
import Data.List


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


upper :: String -> String
upper s = toUpper <$> s
