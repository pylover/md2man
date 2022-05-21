module CLI (Args(..), parseArgs) where


import Data.Version (showVersion)
import Options.Applicative

import Paths_md2man(version)


data Args = Args
  { input :: FilePath
  , output :: FilePath
  , name :: String
  , section :: Int
  , author :: String
  , email :: String
  }
  deriving Show


versionParser :: Parser (a -> a)
versionParser = infoOption (showVersion version)
  (  long "version"
  <> short 'V'
  <> help "Print version information" )


inputParser :: Parser FilePath
inputParser = strArgument
  ( metavar "FILENAME"
 <> value "-"
 <> showDefaultWith (const "Standard Input")
 <> help "Input file.")


outputParser :: Parser FilePath
outputParser = strOption
  ( long "output"
 <> short 'o'
 <> metavar "FILENAME"
 <> value "-"
 <> showDefaultWith (const "Standard Output")
 <> help "Output filename.")


nameParser :: Parser String
nameParser = strOption
  ( long "name"
 <> metavar "NAME"
 <> help "The name for manual page."
  )


sectionParser :: Parser Int
sectionParser = option auto 
  ( long "section"
 <> metavar "NUMBER"
 <> showDefault
 <> help "1-9, Unix manual section number. see `man man`."
  )


authorParser :: Parser String
authorParser = strOption
  ( long "author"
 <> metavar "NAME"
 <> help "Author name."
  )


emailParser :: Parser String
emailParser = strOption
  ( long "email"
 <> metavar "EMAIL"
 <> help "Author email address."
  )


appInfo :: InfoMod s
appInfo = fullDesc
       <> progDesc "Generate Unix manual from markdown."
       <> header "Generate column based text editor"


args :: Parser Args
args = Args 
   <$> inputParser 
   <*> outputParser 
   <*> nameParser
   <*> sectionParser
   <*> authorParser
   <*> emailParser


parseArgs :: IO Args
parseArgs = customExecParser (prefs helpShowGlobals) opts
  where opts = info (args <**> versionParser <**> helper) appInfo
