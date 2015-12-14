
import JPrelude
import Data.Json
import Data.Json.Path
import Data.Json.Pretty

import Options

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

--printFlat :: [(Path, Primitive)]
printFlat = mapM_ (putStrLn.T.unpack.pretty)

pprint = L.putStrLn . encode

main = do
  o <- getOptions
  let instream = case fileArg o of
                      Nothing -> L.getContents
                      Just "-" -> L.getContents
                      Just x -> L.readFile x
  parsed <- parseLazyByteString value <$> instream
  let postprocess = if flatArg o
                       then printFlat . flattenJson
                       else pprint
  case parsed of
    [p] -> postprocess (filterJsonWithGlob (exprArg o) p)
    _ -> error "Failed to parse JSON"
