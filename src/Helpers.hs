module Helpers
  ( trim
  , upper
  , decorate
  , today
  ) where


import Data.Char
import Data.List
import Data.Time


today :: IO String
today = do
  now <- getCurrentTime
  return $ formatTime defaultTimeLocale "%Y-%m-%d" now


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


upper :: String -> String
upper s = toUpper <$> s


decorate :: Char -> String -> String
decorate c s  = c : s ++ [c]
