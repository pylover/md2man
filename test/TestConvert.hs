{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestConvert (htf_thisModulesTests, main) where

import Markdown2Man

import Test.Framework
import System.IO
import System.Process


main = htfMain htf_thisModulesTests

con :: String -> IO String
con i = do
  (ir, iw) <- createPipe
  (or, ow) <- createPipe
  hPutStrLn iw i
  convert ir ow
  o <- hGetContents or
  return $ init o


test_convert = do
  con "" >>= assertEqual ""
  con "\n" >>= assertEqual "\n"
  con "#foo" >>= assertEqual ".TH FOO"
  con "# foo" >>= assertEqual ".TH FOO"
