module DataJsonPathSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import JPrelude
import Data.Json.Path

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

main :: IO ()
main = hspec spec

inv_paths = [ ".a.b.c"
            , "[0].c"
            , "[\"asdf...\"][0][123][\"123\"]"
            , ".asdf_fds"
            , "[\"_\"]"]

readJsonFile :: String -> IO [Value]
readJsonFile f = parseLazyByteString value <$> L.readFile f

spec :: Spec
spec =
  describe "Data.Json.Path" $ do
    it "Invertible paths" $ do
      [data'] <- readJsonFile "test/test-files/csexample.json"
      mapM_ (\x -> (toText <$> readPath x) `shouldBe` Right (T.pack x) ) inv_paths
