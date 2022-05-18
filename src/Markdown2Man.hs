module Markdown2Man
    ( loopLines
    ) where


import System.IO 
  ( Handle
  , hPutStr
  , hGetLine
  , hIsEOF
  )
import Control.Monad.State


data ConState = ConState 
  { inFile :: Handle
  , outFile :: Handle
  , lineNo :: Int
  }
  deriving (Eq, Show)
type Convert a = StateT ConState IO a


loopLines :: Handle -> Handle -> IO ()
loopLines i o = do
  isClosed <- hIsEOF i
  if isClosed
    then return ()
    else do
      l <- hGetLine i
      hPutStr o $ feedLine l
      loopLines i o


-- loopLines :: Handle -> Handle -> IO ()
-- loopLines i o = do
--   isClosed <- hIsEOF i
--   if isClosed
--     then return ()
--     else do
--       l <- hGetLine i
--       hPutStr o $ feedLine l
--       loopLines i o

     
feedLine :: String -> String
feedLine i = i ++ "\r\n"


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

