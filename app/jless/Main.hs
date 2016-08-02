

import JPrelude
import Data.Json
import Data.Json.Path
import Data.Json.Pretty (pretty)

import Program
import Options

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

main = getOptions >>= \opts -> do
  stream <- L.readFile (fileArg opts)
  case decode stream :: Maybe J of
    Just o -> runProgram o *> pure ()
    Nothing -> putStrLn "Invalid JSON"
