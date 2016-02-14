module Options where

import Data.Json.Path
import Options.Applicative

data Options = Options { exprArg :: GlobPath
                       , flatArg :: Bool
                       , prettyArg :: Bool
                       , valuesArg :: Bool
                       , fileArg :: Maybe String }

getOptions :: IO Options
getOptions = execParser opts

opts :: ParserInfo Options
opts = info (helper <*> opts') fullDesc
  where opts' = Options <$> argument glob (metavar "EXPR" <> help "Projection glob")
                        <*> switch (short 'f' <> long "flat" <> help "Flatten result")
                        <*> switch (short 'p' <> long "pretty-print" <> help "Pretty print result")
                        <*> switch (short 'v' <> long "values-only" <> help "Output just values")
                        <*> optional (argument str (metavar "FILE" <> help "Input file"))
        glob = do
          s <- readGlobPath <$> str
          case s of
            Left x -> error (show x)
            Right x -> pure x
