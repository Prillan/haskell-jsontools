
import JPrelude
import Data.Json
import Data.Json.Path
import Data.Json.Pretty (pretty, prettyRaw)

import Options

import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T

printEntry printer (p, t) = printer p <> ": " <> pretty t

printFlat printer = mapM_ (putStrLn.T.unpack.printEntry printer)
printValues x = mapM_ (putStrLn.T.unpack.prettyRaw) (x ^.. deep _Primitive)

main = do
  o <- getOptions
  let instream = case fileArg o of
                      Nothing -> L.getContents
                      Just "-" -> L.getContents
                      Just x -> L.readFile x
  parsed <- parseLazyByteString value <$> instream
  let postprocess = case (flatArg o, prettyArg o, valuesArg o) of
                      (_, True, _) -> L.putStrLn . encodePretty
                      (True, _, _) -> printFlat printer . flattenJson
                      (_, _, True) -> printValues
                      (_, _, _)    -> L.putStrLn . encode
      printer = if useJsonPointer o
                   then rfc6901pointer
                   else pretty
  case parsed of
    [p] -> postprocess (filterJsonWithGlob (exprArg o) p)
    _ -> error "Failed to parse JSON"
