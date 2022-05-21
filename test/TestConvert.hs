{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}
module TestConvert (htf_thisModulesTests, main) where

import MD2Man

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


defaultOptions = Options "foo" 1 "Alice" "alice@exmample.com"


con :: String -> IO String
con i = do
  iknob <- newKnob (packStr (i))
  ih <- newFileHandle iknob "foo" ReadMode
  
  oknob <- newKnob (pack [])
  oh <- newFileHandle oknob "bar" WriteMode

  convert defaultOptions ih oh
  hClose ih
  hClose oh
  bo <- Data.Knob.getContents oknob
  return $ T.unpack (decodeUtf8 bo)


test_convert = do
  con "" >>= assertEqual ""
  con "\n" >>= assertEqual ""


test_convert_title = do
  con "#foo" >>= assertEqual ".TH FOO 1\n"
  con "# foo" >>= assertEqual ".TH FOO 1\n"


test_convert_section = do
  con "##foo" >>= assertEqual ".SH FOO\n"
  con "## foo" >>= assertEqual ".SH FOO\n"


test_convert_paragraph = do
  con "#foo\n\
    \\n\
    \\n" >>= assertEqual ".TH FOO 1\n"

  con "#foo\n\
    \bar baz\n\
    \\n\
    \qux quux\n">>= assertEqual ".TH FOO 1\n\
    \bar baz\n\
    \.PP\n\
    \qux quux\n"

  con "#foo\n\
    \bar baz\n\
    \\n\
    \\n\
    \\n\
    \qux quux\n">>= assertEqual ".TH FOO 1\n\
    \bar baz\n\
    \.PP\n\
    \qux quux\n"


test_convert_bold = do
  con "#foo\n\
    \** bar **\n" >>= assertEqual ".TH FOO 1\n\
    \\\fB bar \\fR\n"

  con "#foo\n\
    \** bar\n\
    \ baz **\n" >>= assertEqual ".TH FOO 1\n\
    \\\fB bar\n\
    \ baz \\fR\n"


test_convert_italic = do
  con [r|#foo
* bar *
|] >>= assertEqual [r|.TH FOO 1
\fI bar \fR
|]


test_convert_full = do
  con "#foo\n\
    \bar baz\n\
    \## Qux\n\
    \Qux is asesome." >>= assertEqual ".TH FOO 1\n\
    \bar baz\n\
    \.SH QUX\n\
    \Qux is asesome.\n"
