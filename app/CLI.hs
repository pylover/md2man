module CLI (Args(..), parseArgs) where


import Data.Version (showVersion)
import Options.Applicative

import Paths_markdown2man(version)


data Args = Args
  { input :: FilePath
  , output :: FilePath
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
 <> help "Input file. otherwise standard input will be choosen.")


outputParser :: Parser FilePath
outputParser = strOption
  ( long "output"
 <> short 'o'
 <> metavar "FILENAME"
 <> value "-"
 <> help "Output filename, default: standard output")


appInfo :: InfoMod s
appInfo = fullDesc
       <> progDesc "Generate Unix manual from markdown."
       <> header "Generate column based text editor"


args :: Parser Args
args = Args 
   <$> inputParser 
   <*> outputParser 


parseArgs :: IO Args
parseArgs = customExecParser (prefs helpShowGlobals) opts
  where opts = info (args <**> versionParser <**> helper) appInfo
