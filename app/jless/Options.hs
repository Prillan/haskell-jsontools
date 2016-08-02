module Options where

import Data.Json.Path
import Options.Applicative

data Options = Options { fileArg :: String }

getOptions = execParser opts

opts = info (helper <*> opts') fullDesc
  where opts' = Options <$> argument str (metavar "FILE" <> help "Input file")
        glob = do
          s <- readGlobPath <$> str
          case s of
            Left x -> error (show x)
            Right x -> pure x
