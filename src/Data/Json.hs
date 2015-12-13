module Data.Json (flattenJson) where

import JPrelude
import Data.Json.Path

import qualified Data.HashMap.Strict as H

flattenJson :: Value -> [(Path, Value)]
flattenJson = flattenJson' mempty
  where flattenJson' p (Object o) =
          concatMap (\(k, e) -> flattenJson' (p <> property k) e) $ H.toList o
        flattenJson' p (Array a) =
          concat $ zipWith (\i e -> flattenJson' (p <> index i) e) [0..] (toList a)
        flattenJson' p v = [(p, v)]
