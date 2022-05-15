module Options where

import Options.Applicative as O

data Options = Options { file :: FilePath } deriving Show

optsParser :: Parser Options
optsParser = Options
  <$> O.argument O.str (metavar "FILE.eventlog")

opts :: ParserInfo Options
opts = info (optsParser <**> helper)
      ( fullDesc
     <> progDesc "Generate a speedscope.app json file from an eventlog"
     <> O.header "hs-speedscope" )
