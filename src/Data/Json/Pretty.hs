{-# LANGUAGE FlexibleInstances #-}
module Data.Json.Pretty where

import JPrelude

import Data.Json.Path

import Data.Aeson.Lens
import Data.Text (Text, pack)

class Pretty a where
  pretty :: a -> Text

instance Pretty Path where
  pretty = toText

instance Pretty (Path, String) where
  pretty (p, s) = pretty (p, pack s)
instance Pretty (Path, Text) where
  pretty (p, t) = pretty p <> ": " <> t
instance Pretty (Path, Primitive) where
  pretty (p, StringPrim x) = pretty (p, x)
  pretty (p, NumberPrim n) = pretty (p, show n)
  pretty (p, BoolPrim b) = pretty (p, if b then "true" :: Text else "false")
  pretty (p, NullPrim) = pretty (p, "null" :: Text)
