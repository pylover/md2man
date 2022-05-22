{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestConvert (htf_thisModulesTests, main) where

import MD2Man

import Test.Framework
import System.IO (IOMode(..), hClose)
import qualified Data.Text as T
import Data.ByteString (pack, ByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Knob


main = htfMain htf_thisModulesTests


packStr :: String -> ByteString
packStr = encodeUtf8 . T.pack


minimalOptions = Options 1 "" ""
fullOptions = Options 1 "Alice" "alice@exmample.com"


con :: Options -> String -> IO String
con opts i = do
  iknob <- newKnob (packStr (i))
  ih <- newFileHandle iknob "foo" ReadMode
  
  oknob <- newKnob (pack [])
  oh <- newFileHandle oknob "bar" WriteMode

  convert opts ih oh
  hClose ih
  hClose oh
  bo <- Data.Knob.getContents oknob
  return $ T.unpack (decodeUtf8 bo)


mini = con minimalOptions
full = con fullOptions


test_convert = do
  mini "" >>= assertEqual ""
  mini "\n" >>= assertEqual ""


test_convert_title = do
  mini "#foo" >>= assertEqual ".TH FOO 1\n"
  mini "# foo" >>= assertEqual ".TH FOO 1\n"


test_convert_section = do
  mini "##foo" >>= assertEqual ".SH FOO\n"
  mini "## foo" >>= assertEqual ".SH FOO\n"


test_convert_paragraph = do
  mini "#foo\n\
    \\n\
    \\n" >>= assertEqual ".TH FOO 1\n"

  mini "#foo\n\
    \bar baz\n\
    \\n\
    \qux quux\n">>= assertEqual ".TH FOO 1\n\
    \bar baz\n\
    \.PP\n\
    \qux quux\n"

  mini "#foo\n\
    \bar baz\n\
    \\n\
    \\n\
    \\n\
    \qux quux\n">>= assertEqual ".TH FOO 1\n\
    \bar baz\n\
    \.PP\n\
    \qux quux\n"


test_convert_bold = do
  mini "#foo\n\
    \** bar **\n" >>= assertEqual ".TH FOO 1\n\
    \\\fB bar \\fR\n"

  mini "#foo\n\
    \** bar\n\
    \ baz **\n" >>= assertEqual ".TH FOO 1\n\
    \\\fB bar\n\
    \ baz \\fR\n"


test_convert_italic = do
  mini "* bar *" >>= assertEqual "\\fI bar \\fR\n"


test_convert_full = do
  full "\
    \#foo\n\
    \bar baz\n\
    \## Qux\n\
    \Qux is asesome" >>= assertEqual "\
    \.TH FOO 1\n\
    \bar baz\n\
    \.SH QUX\n\
    \Qux is asesome\n\
    \.SH AUTHOR\n\
    \Alice (alice@exmample.com)\n"
