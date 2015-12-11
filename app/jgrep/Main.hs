import Data.Array ((!), Array)
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as H
import Data.JsonStream.Parser
import qualified Data.ByteString.Lazy as B
import Data.Foldable (forM_)
import Data.Monoid ((<>))
import qualified Data.Vector as V
import Options.Applicative (info, helper, argument, str, metavar, help, execParser, fullDesc, switch, short)
import Data.Text (unpack)
import Text.Regex.TDFA
import System.Console.ANSI

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
                           (V.toList a)
        findMatches' p (Object o) =
          concatMap (\(k, e) -> findMatches' (p ++ "." ++ (unpack k)) e) $ H.toList o
        fromString :: String -> String -> [Match]
        fromString p val = map (fromMatchArray p val) matches
          where matches = matchAll r val

printMatch (Match p v (s, c)) = do
  setSGR [SetColor Foreground Vivid Magenta]
  putStr p
  setSGR [Reset]
  putStr ": "
  putStr (take s v)
  setSGR [SetColor Foreground Vivid Green]
  putStr (take c $ drop s v)
  setSGR [Reset]
  putStrLn (drop (s+c) v)

main = do
   stdin <- B.getContents
   let result = parseLazyByteString value stdin :: [Value]
   o <- execParser opts
   let matches = concatMap (findMatches (regexArg o)) result
   if verboseArg o then print matches else pure ()

   mapM_ printMatch matches



data Options = Options { regexArg :: Regex, verboseArg :: Bool }
opts = info ( helper
              <*> (Options
                   <$> (argument (makeRegex <$> str)
                                 (metavar "EXPRESSION" <> help "Expression"))
                   <*> (switch (short 'v' <> help "Verbose")))
            )
       ( fullDesc )
