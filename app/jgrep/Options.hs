module Options (getOptions, Options(..)) where

import Options.Applicative
import Text.Regex.TDFA (Regex, makeRegex)

data Options = Options { regexArg :: Regex, verboseArg :: Bool }

getOptions = execParser opts
opts = info ( helper
              <*> (Options
                   <$> (argument (makeRegex <$> str)
                                 (metavar "EXPRESSION" <> help "Expression"))
                   <*> (switch (short 'v' <> help "Verbose")))
            )
       ( fullDesc )
