module Main where


import System.IO 
  ( IOMode(..)
  , Handle
  , withFile
  , stdin
  , stdout
  )

import CLI
import Markdown2Man


main :: IO ()
main = do
  Args ifn ofn <- parseArgs
  withFile_ ifn ReadMode 
    (\i -> withFile_ ofn WriteMode 
      (\o -> convert i o))


withFile_ :: String -> IOMode -> (Handle -> IO r) -> IO r
withFile_ "-" ReadMode f = f stdin
withFile_ "-" WriteMode f = f stdout
withFile_ fn mode f = withFile fn mode f
