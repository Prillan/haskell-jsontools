module Data.Json (flattenJson, filterJson, filterJsonWithGlob, isPrimitive) where

import Control.Lens (plate, (^..), cosmos)
import Data.Aeson.Lens

import JPrelude
import Data.Json.Path

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H

flattenJson :: Value -> [(Path, Primitive)]
flattenJson = flattenJson' mempty
  where flattenJson' p (Object o) =
          concatMap (\(k, e) -> flattenJson' (p <> property k) e) $ H.toList o
        flattenJson' p (Array a) =
          concat $ zipWith (\i e -> flattenJson' (p <> index i) e) [0..] (toList a)
        flattenJson' p (String s) = [(p, StringPrim s)]
        flattenJson' p (Null) = [(p, NullPrim)]
        flattenJson' p (Number n) = [(p, NumberPrim n)]
        flattenJson' p (Bool b) = [(p, BoolPrim b)]

filterJson :: (Path -> Value -> Bool) -> Value -> Value
filterJson f v
  | isPrimitive v && not (f mempty v) = Null
  | otherwise = filterJson' f mempty v

filterJsonWithGlob :: GlobPath -> Value -> Value
filterJsonWithGlob glob = filterJson (\p v -> not (null $ v^..cosmos._Primitive))
                        . filterJson (\p v -> (not $ isPrimitive v) || matches p glob)

isPrimitive :: Value -> Bool
isPrimitive (Object _) = False
isPrimitive (Array _) = False
isPrimitive _ = True

filterJson' :: (Path -> Value -> Bool) -> Path -> Value -> Value
filterJson' f p (Object o) =
  Object
  . H.filterWithKey (\k v -> f (p <> property k) v)
  . H.mapWithKey (\k v -> filterJson' f (p <> property k) v) $ o
filterJson' f p (Array a) =
  Array
  . V.map snd
  . V.filter (\(i, e) -> f (p <> index i) e)
  . V.map (\(i, e) -> (i, filterJson' f (p <> index i) e))
  . V.map (\(i, e) -> (toInteger i, e))
  . V.indexed $ a
filterJson' f p v = v
