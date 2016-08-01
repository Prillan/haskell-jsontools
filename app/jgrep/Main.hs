module Main (main) where

import JPrelude

import Data.Json.Path

import Options

import Data.Array ((!))
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy as B
import Data.Text (Text, unpack)
import System.Console.ANSI
import Text.Regex.TDFA

data Match = Match Path String (Int, Int)
  deriving Show

fromMatchArray p v a = Match p v (a ! 0)

findMatches :: Regex -> Value -> [Match]
findMatches r = findMatches' mempty
  where findMatches' :: Path -> Value -> [Match]
        findMatches' p Null = fromString p "null"
        findMatches' p (Number n) = fromString p (show n)
        findMatches' p (String s) = fromString p (unpack s)
        findMatches' p (Bool b) = fromString p (if b then "true" else "false")
        findMatches' p (Array a) =
          concat $ zipWith (\i e -> findMatches' (p <> index i) e)
                           [0..]
                           (toList a)
        findMatches' p (Object o) =
          concatMap (\(k, e) -> findMatches' (p <> property k) e) $ H.toList o
        fromString :: Path -> String -> [Match]
        fromString p val = map (fromMatchArray p val) matches
          where matches = matchAll r val

printColored :: Color -> String -> IO ()
printColored c s = setSGR [SetColor Foreground Vivid c] >> putStr s >> setSGR [Reset]

printMatch :: (Path -> Text) -> Match -> IO ()
printMatch printer (Match p v (s, c)) = do
  printColored Magenta (unpack $ printer p)
  putStr ": "
  putStr (take s v)
  printColored Green (take c $ drop s v)
  putStrLn (drop (s+c) v)

main :: IO ()
main = do
   stdin <- B.getContents
   let result = parseLazyByteString value stdin :: [Value]
   o <- getOptions
   let matches = concatMap (findMatches (regexArg o)) result
   if verboseArg o then print matches else pure ()

   let printer = if useJsonPointer o
                    then rfc6901pointer
                    else toText
   mapM_ (printMatch printer) matches
