
import JPrelude
import Data.Json
import Data.Json.Path
import Data.Json.Pretty (pretty)

import Options

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

printFlat = mapM_ (putStrLn.T.unpack.pretty)
printValues x = mapM_ (putStrLn.T.unpack.pretty) (x ^.. deep _Primitive)

main = do
  o <- getOptions
  let instream = case fileArg o of
                      Nothing -> L.getContents
                      Just "-" -> L.getContents
                      Just x -> L.readFile x
  parsed <- parseLazyByteString value <$> instream
  let postprocess = case (flatArg o, prettyArg o, valuesArg o) of
                      (_, True, _) -> L.putStrLn . encodePretty
                      (True, _, _) -> printFlat . flattenJson
                      (_, _, True) -> printValues
                      (_, _, _)    -> L.putStrLn . encode
  case parsed of
    [p] -> postprocess (filterJsonWithGlob (exprArg o) p)
    _ -> error "Failed to parse JSON"
