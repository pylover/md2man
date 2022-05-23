module CLI (Args(..), parseArgs) where


import Data.Version (showVersion)
import Options.Applicative

import Paths_md2man(version)


data Args = Args
  { input :: FilePath
  , output :: FilePath
  , section :: Int
  , author :: String
  , email :: String
  , appVersion :: String
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


sectionParser :: Parser Int
sectionParser = option auto 
  ( long "section"
 <> metavar "NUMBER"
 <> showDefault
 <> value 1
 <> help "1-9, Unix manual section number. see `man man`."
  )


appVersionParser :: Parser String
appVersionParser = strOption
  ( long "app-version"
 <> metavar "VERSION"
 <> showDefault
 <> value ""
 <> help "Application version"
  )


authorParser :: Parser String
authorParser = strOption
  ( long "author"
 <> metavar "NAME"
 <> value ""
 <> help "Author name."
  )


emailParser :: Parser String
emailParser = strOption
  ( long "email"
 <> metavar "EMAIL"
 <> value ""
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
   <*> sectionParser
   <*> authorParser
   <*> emailParser
   <*> appVersionParser


parseArgs :: IO Args
parseArgs = customExecParser (prefs helpShowGlobals) opts
  where opts = info (args <**> versionParser <**> helper) appInfo
