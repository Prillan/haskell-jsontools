
import JPrelude
import Data.Json
import Data.Json.Path

import Options

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

project :: GlobPath -> Value -> [(Path, Value)]
project p v = filter (\(p', v') -> matches p' p) flat
  where flat = flattenJson v

pprint :: (Path, Value) -> IO ()
pprint (p, v) = do
  putStr . T.unpack $ toText p <> " "
  L.putStrLn (encode v)

main = do
  o <- getOptions
  let instream = case fileArg o of
                      Nothing -> L.getContents
                      Just "-" -> L.getContents
                      Just x -> L.readFile x
  parsed <- parseLazyByteString value <$> instream
  case parsed of
    [p] -> mapM_ pprint (project (exprArg o) p)
    _ -> error "Failed to parse JSON"
