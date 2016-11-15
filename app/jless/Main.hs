

import JPrelude
import Data.Json
import Data.Json.Path
import Data.Json.Pretty (pretty)

import Program
import Options

import FFIHAXX (reopenStdin)

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import System.IO

main = getOptions >>= \opts -> do
  stream <- case fileArg opts of
              "-" -> do
                fakeStdin <- openFile "/dev/stdin" ReadMode
                s <- S.hGetContents fakeStdin
                _ <- reopenStdin
                pure (L.fromStrict s)
              f   -> L.readFile f
  case decode stream :: Maybe J of
    Just o -> runProgram o *> pure ()
    Nothing -> putStrLn "Invalid JSON"
