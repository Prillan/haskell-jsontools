module Options (getOptions, Options(..)) where

import Options.Applicative
import Text.Regex.TDFA (Regex, makeRegex)

data Options = Options { regexArg :: Regex
                       , verboseArg :: Bool
                       , useJsonPointer :: Bool}

getOptions = execParser opts
opts = info ( helper
              <*> (Options
                   <$> (argument (makeRegex <$> str)
                                 (metavar "EXPRESSION" <> help "Expression"))
                   <*> (switch (short 'v' <> help "Verbose"))
                   <*> (switch (long "use-json-pointer" <> help "Use RFC6901 Json Pointers")))
            )
       ( fullDesc )
