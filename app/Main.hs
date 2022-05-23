module Main where


import System.IO 
  ( IOMode(..)
  , Handle
  , withFile
  , stdin
  , stdout
  )

import CLI
import MD2Man
import Helpers


main :: IO ()
main = do
  Args ifn ofn section author email ver book <- parseArgs
  d <- today
  let options = Options section author email d ver book
  
  withFile_ ifn ReadMode 
    (\i -> withFile_ ofn WriteMode 
      (\o -> convert options i o))


withFile_ :: String -> IOMode -> (Handle -> IO r) -> IO r
withFile_ "-" ReadMode f = f stdin
withFile_ "-" WriteMode f = f stdout
withFile_ fn mode f = withFile fn mode f
