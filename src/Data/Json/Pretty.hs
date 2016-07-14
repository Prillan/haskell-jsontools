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
  pretty (p, StringPrim x) = pretty (p, show x)
  pretty (p, NumberPrim n) = pretty (p, show n)
  pretty (p, BoolPrim b) = pretty (p, if b then "true" :: Text else "false")
  pretty (p, NullPrim) = pretty (p, "null" :: Text)


tshow :: Show a => a -> Text
tshow = pack.show

instance Pretty Primitive where
  pretty (StringPrim x) = tshow x
  pretty (NumberPrim x) = tshow x
  pretty (BoolPrim x) = if x then "true" else "false"
  pretty (NullPrim) = "null"


class PrettyRaw a where
  prettyRaw :: a -> Text

instance PrettyRaw Primitive where
  prettyRaw (StringPrim x) = x
  prettyRaw (NumberPrim x) = tshow x
  prettyRaw (BoolPrim x) = if x then "true" else "false"
  prettyRaw (NullPrim) = "null"
