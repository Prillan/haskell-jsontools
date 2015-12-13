{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Json.Path (Path,
                       toText,
                       property,
                       index,
                       readPath) where

import JPrelude

import Data.Coerce
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Text.JSON.Parsec (p_string)
import Text.Parsec

tshow :: Show a => a -> Text
tshow = T.pack . show

data PathElement = Property Text | Index Integer
  deriving (Show, Eq)
newtype Path = Path [PathElement]
  deriving (Show, Monoid)

data GlobPathElement = GPEStar | GPEChoice [GlobPath]  | GPE PathElement
  deriving Show
newtype GlobPath = GlobPath [GlobPathElement]
  deriving Show

test x =
  let
    parsed = readGlobPath x
  in
    do
      print (gpToText <$> parsed)
      mapM_ (mapM_ (putStrLn.T.unpack)) (map gpToText . flatten <$> parsed)

matches :: Path -> GlobPath -> Bool
matches p gp = any (matches' p') gps
  where p' = coerce p :: [PathElement]
        gps = coerce (flatten gp) :: [[GlobPathElement]]
        matches' :: [PathElement] -> [GlobPathElement] -> Bool
        matches' [] [] = True
        matches' [] _  = False
        matches' _  [] = False
        matches' (x:xs) ((GPE y):ys) = if x == y then matches' xs ys else False
        matches' (x:xs) (GPEStar:ys) = matches' xs ys

gpeToText :: GlobPathElement -> Text
gpeToText GPEStar = ".*"
gpeToText (GPEChoice paths) = "{" <> T.intercalate "," (map gpToText paths) <> "}"
gpeToText (GPE elem) = peToText elem
gpToText (GlobPath xs) = T.concat (map gpeToText xs)

flatten :: GlobPath -> [GlobPath]
flatten (GlobPath xs) = coerce (flatten' xs)
flatten' :: [GlobPathElement] -> [[GlobPathElement]]
flatten' [] = [[]]
flatten' ((GPEChoice paths):xs) =  (++) <$> subpaths <*> flatten' xs
  where subpaths = concatMap flatten' (map coerce paths)
flatten' (x:xs) = ([x] ++) <$> (flatten' xs)

singleton pe = Path [pe]
property t = singleton (Property t)
index i = singleton (Index i)

peToText (Property t)
  | isGood t = "." <> t
  | otherwise = "[" <> tshow t <> "]"
peToText (Index i) = "[" <> tshow i <> "]"

toText :: Path -> Text
toText (Path xs) = T.concat (map peToText xs)

good = ['A'..'Z'] ++ ['a'..'z'] ++ "0123456789_"
isGood t
  | T.null t  = True
  | otherwise = T.head t `elem` (['A'..'Z'] ++ ['a'..'z'])
              && T.all (`elem` good) (T.tail t)

readPath :: String -> Either ParseError Path
readPath = parse path ""
  where
        path = Path <$> (many element <* eof)
        element = prop_good
                <|> try prop
                <|> index'
        propname_good = T.pack <$> (many1 . oneOf $ good)
        prop_good = Property <$> (char '.' *> propname_good)
        prop = Property . T.pack <$> between (char '[') (char ']') (p_string)
        index' = Index . read <$> between (char '[') (char ']') (many1 digit)

readGlobPath :: String -> Either ParseError GlobPath
readGlobPath = parse path ""
  where
        path = GlobPath <$> (many element)
        element = try (string ".*" *> pure GPEStar)
                <|> try (GPEChoice <$> between (char '{') (char '}') (sepBy1 path (char ',')))
                <|> try (GPE <$> prop_good)
                <|> try (GPE <$> try prop)
                <|> try (GPE <$> index')
        propname_good = T.pack <$> (many1 . oneOf $ good)
        prop_good = Property <$> (char '.' *> propname_good)
        prop = Property . T.pack <$> between (char '[') (char ']') (p_string)
        index' = Index . read <$> between (char '[') (char ']') (many1 digit)
