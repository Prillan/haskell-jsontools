module JPrelude
       ( module Data.Aeson
       , module Data.Foldable
       , module Data.Monoid
       , (^..), deep, _Primitive
       , parseLazyByteString, value, genericLength, encodePretty) where

import Control.Lens
import Control.Lens.Plated

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Lens
import Data.Foldable
import Data.Monoid
import Data.JsonStream.Parser (parseLazyByteString, value)

import Data.List (genericLength)
