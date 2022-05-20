{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}
module TestConvert (htf_thisModulesTests, main) where

import Markdown2Man

import Test.Framework
import System.IO (IOMode(..), hClose)
import qualified Data.Text as T
import Data.ByteString (pack, ByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Knob
import Text.RawString.QQ


main = htfMain htf_thisModulesTests


packStr :: String -> ByteString
packStr = encodeUtf8 . T.pack


con :: String -> IO String
con i = do
  iknob <- newKnob (packStr (i ++ "\n"))
  ih <- newFileHandle iknob "foo" ReadMode
  
  oknob <- newKnob (pack [])
  oh <- newFileHandle oknob "bar" WriteMode

  convert ih oh
  hClose ih
  hClose oh
  bo <- Data.Knob.getContents oknob
  return $ init ( T.unpack (decodeUtf8 bo))


test_convert = do
  con "" >>= assertEqual ""
  con "\n" >>= assertEqual "\n"


test_convert_title = do
  con "#foo" >>= assertEqual ".TH FOO"
  con "# foo" >>= assertEqual ".TH FOO"


test_convert_section = do
  con "##foo" >>= assertEqual ".SH FOO"
  con "## foo" >>= assertEqual ".SH FOO"


test_convert_full = do
  con [r|#foo|] >>= assertEqual ".TH FOO"
