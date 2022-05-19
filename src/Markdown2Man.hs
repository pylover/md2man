module Markdown2Man
    ( loopLines
    ) where


import System.IO 
import Control.Monad.State

import Helpers
import Tokenizer


data ConState = ConState 
  { outFile :: Handle
  , lineNo :: Int
  }
  deriving (Eq, Show)
type ConvertT a = StateT ConState IO a


loopLines :: Handle -> Handle -> IO ()
loopLines i o = evalStateT (loopLines_ i) (ConState o 0)


loopLines_ :: Handle -> ConvertT ()
loopLines_ i = do
  isClosed <- lift $ hIsEOF i
  if isClosed
    then return ()
    else readLine >>= feedLine . tokenize >> modify' nextLine >> loopLines_ i
  where
    readLine = lift $ hGetLine i


nextLine :: ConState -> ConState
nextLine (ConState o l) = ConState o (l + 1)


out :: String -> ConvertT ()
out s = gets outFile >>= lift . (\h -> hPutStr h s)


outLn :: String -> ConvertT ()
outLn s = gets outFile >>= lift . (\h -> hPutStrLn h s)


outListLn :: [String] -> ConvertT ()
outListLn = outLn . unwords


feedLine :: [String] -> ConvertT ()
feedLine ("#":xs) = outListLn $ ".SH " : (upper <$> xs)
feedLine xs = outListLn xs


-- .TH                Title
-- .HS NAME           Name
-- .SH SYNOPSIS       Synopsis
-- .SH DESCRIPTION
-- .SH OPTIONS
-- .SH EXAMPLES
-- .B                 Bold
-- .I                 Italic
-- .R                 Normal
-- .PP                Paragraph
-- .TP                Indent all lines but the first!
-- .BR                First word -> bold
-- .\"                Comment


-- .nf turns off paragraph filling mode: we don’t want that for showing command lines.
-- .fi turns it back on.
-- .RS starts a relative margin indent: examples are more visually distinguishable if they’re indented.
-- .RE ends the indent.
-- \\ puts a backslash in the output. Since troff uses backslash for fonts and other in-line commands, it needs to be doubled in the manual page source so that the output has one.

