module Main (main) where

import Options

import Data.Array ((!))
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as H
import Data.JsonStream.Parser
import qualified Data.ByteString.Lazy as B
import Data.Foldable (toList)
--import Data.Monoid ((<>))
import Data.Text (unpack)
import System.Console.ANSI
import Text.Regex.TDFA

type Path = String
data Match = Match Path String (Int, Int)
  deriving Show
fromMatchArray p v a = Match p v (a ! 0)

findMatches :: Regex -> Value -> [Match]
findMatches r = findMatches' "/"
  where findMatches' :: String -> Value -> [Match]
        findMatches' p Null = fromString p "null"
        findMatches' p (Number n) = fromString p (show n)
        findMatches' p (String s) = fromString p (unpack s)
        findMatches' p (Bool b) = fromString p (if b then "true" else "false")
        findMatches' p (Array a) =
          concat $ zipWith (\i e -> findMatches' (p ++ "[" ++ show i ++ "]") e)
                           [0..]
                           (toList a)
        findMatches' p (Object o) =
          concatMap (\(k, e) -> findMatches' (p ++ "." ++ (unpack k)) e) $ H.toList o
        fromString :: String -> String -> [Match]
        fromString p val = map (fromMatchArray p val) matches
          where matches = matchAll r val

printColored c s = setSGR [SetColor Foreground Vivid c] >> putStr s >> setSGR [Reset]

printMatch (Match p v (s, c)) = do
  printColored Magenta p
  putStr ": "
  putStr (take s v)
  printColored Green (take c $ drop s v)
  putStrLn (drop (s+c) v)

main = do
   stdin <- B.getContents
   let result = parseLazyByteString value stdin :: [Value]
   o <- getOptions
   let matches = concatMap (findMatches (regexArg o)) result
   if verboseArg o then print matches else pure ()

   mapM_ printMatch matches
