module MD2Man
    ( convert
    , Options(..)
    ) where


import System.IO 
import Text.Printf
import Control.Monad.State

import Helpers


data Options = Options
  { section :: Int
  , author :: String
  , email :: String
  , date :: String
  , version :: String
  }
  deriving (Eq, Show)


data Style = Normal | Bold | Italic | Code
  deriving (Eq, Show)


data Status = Title | Section | FirstParagraph | Paragraph
  deriving (Eq, Show)


data ConState = ConState 
  { options :: Options
  , outFile :: Handle
  , lineNo :: Int
  , style :: Style
  , status :: Status
  }
  deriving (Eq, Show)
type ConvertT a = StateT ConState IO a


outChr :: Char -> ConvertT ()
outChr s = gets outFile >>= lift . (\h -> hPutChar h s)


out :: String -> ConvertT ()
out s = gets outFile >>= lift . (\h -> hPutStr h s)


outLn :: String -> ConvertT ()
outLn s = gets outFile >>= lift . (\h -> hPutStrLn h s)


ifthen :: Bool -> ConvertT () -> ConvertT ()
ifthen True x = x
ifthen False _ = return ()


convert :: Options -> Handle -> Handle -> IO ()
convert opts i o = evalStateT (render i) (ConState opts o 1 Normal Title)


render :: Handle -> ConvertT ()
render i = do
  loopLines i
  footer


footer :: ConvertT ()
footer = do
  opts <- gets options
  printAuthor (author opts) (email opts)


printAuthor :: String -> String -> ConvertT ()
printAuthor "" "" = return ()
printAuthor "" e = outLn $ ".SH AUTHOR\n" ++ e
printAuthor a "" = outLn $ ".SH AUTHOR\n" ++ a
printAuthor a e = outLn $ printf ".SH AUTHOR\n%s (%s)" a e
 

loopLines :: Handle -> ConvertT ()
loopLines i = do
  isClosed <- lift $ hIsEOF i
  if isClosed
    then return ()
    else readLine >>= feedLine >> modify' nextLine >> loopLines i
  where
    readLine = lift $ hGetLine i


newStyle :: Style -> ConvertT ()
newStyle s = modify' $ newS
  where newS (ConState opts o l _ st) = ConState opts o l s st


newStatus :: Status -> ConvertT ()
newStatus st = modify' $ newS
  where newS (ConState opts o l s _) = ConState opts o l s st


nextLine :: ConState -> ConState
nextLine (ConState opts o l s st) = ConState opts o (l + 1) s st


feedLine :: String -> ConvertT ()

-- Code
feedLine ('`':'`':'`':xs) = do
  s <- gets style
  case s of 
    Code -> outLn ".EE" >> newStyle Normal
    _ -> outLn ".EX" >> newStyle Code

-- Section
feedLine ('#':'#':xs) = do
  outLn $ ".SH " ++ (upper . trim $ xs)
  newStatus Section

-- Title
feedLine ('#':xs) = do
  opts <- gets options
  let d = date opts
      t = trim xs
      u = upper t
  if d == ""
    then outLn $ printf ".TH %s %d" u (section opts)
    else outLn $ printf ".TH %s %d %s %s %s man page" 
      u (section opts) (date opts) (version opts) t
  newStatus Title

-- Empty line
feedLine "" = do
  s <- gets status
  case s of 
    FirstParagraph -> newStatus Paragraph
    _ -> return ()

-- Normal text
feedLine xs = do
  s <- gets status
  case s of 
    Paragraph -> outLn ".PP"
    Title -> newStatus FirstParagraph
    Section -> newStatus FirstParagraph
    _ -> return () 
  processLine xs


processLine :: String -> ConvertT ()
processLine [] = outLn "" 
processLine ('*':'*':xs) = do 
  s <- gets style
  case s of
    Bold -> out "\\fR"
    _ -> out "\\fB" >> newStyle Bold
  processLine xs
processLine ('*':xs) = do 
  s <- gets style
  case s of
    Italic -> out "\\fR"
    _ -> out "\\fI" >> newStyle Italic
  processLine xs

processLine (x:xs) = outChr x >> processLine xs


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
