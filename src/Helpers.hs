module Helpers
  ( trim
  , upper
  , decorate
  ) where


import Data.Char
import Data.List


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


upper :: String -> String
upper s = toUpper <$> s


decorate :: Char -> String -> String
decorate c s  = c : s ++ [c]
