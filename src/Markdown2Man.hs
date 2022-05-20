module Markdown2Man
    ( convert
    , Options(..)
    ) where


import System.IO 
import Control.Monad.State

import Helpers


data Options = Options
  { name :: String
  , section :: Int
  , author :: String
  , email :: String
  }
  deriving (Eq, Show)


data ConState = ConState 
  { options :: Options
  , outFile :: Handle
  , lineNo :: Int
  }
  deriving (Eq, Show)
type ConvertT a = StateT ConState IO a


convert :: Options -> Handle -> Handle -> IO ()
convert opts i o = evalStateT (loopLines i) (ConState opts o 1)


out :: String -> ConvertT ()
out s = gets outFile >>= lift . (\h -> hPutStr h s)


outLn :: String -> ConvertT ()
outLn s = gets outFile >>= lift . (\h -> hPutStrLn h s)


outListLn :: [String] -> ConvertT ()
outListLn = outLn . unwords


header :: ConvertT ()
header = do
  opts <- gets options
  outLn $ ".\" Manpage for " ++ name opts  ++ "."
  outLn $ ".\" Contact " ++ email opts ++ " to correct errors or typos."

  
loopLines :: Handle -> ConvertT ()
loopLines i = do
  isClosed <- lift $ hIsEOF i
  if isClosed
    then return ()
    else readLine >>= feedLine >> modify' nextLine >> loopLines i
  where
    readLine = lift $ hGetLine i


nextLine :: ConState -> ConState
nextLine (ConState opts o l) = ConState opts o (l + 1)


feedLine :: String -> ConvertT ()

-- Section
feedLine ('#':'#':xs) = outLn $ ".SH " ++ (upper . trim $ xs)

-- Title
feedLine ('#':xs) = outLn $ ".TH " ++ (upper . trim $ xs) ++ " " ++  show 1


feedLine xs = outLn xs


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

