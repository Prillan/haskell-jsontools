module DataJsonPathSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import JPrelude
import Data.Json
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

p ~~ q = (p, q, True)
p /~ q = (p, q, False)
matchings = [ ".player.name" /~ ".player.*.money"
            , ".player.name" ~~ ".player.*"
            , ".player.state.health" ~~ ".**.health"
            , ".player.money" /~ ".**.health"
            , "[0].asdf" ~~ ".*.asdf"]

readJsonFile :: String -> IO [Value]
readJsonFile f = parseLazyByteString value <$> L.readFile f

spec :: Spec
spec = do
  describe "Glob matching" $ do
    forM_ matchings $ \(p, g, b) -> do
      it (show p <> " " <> (if b then "~~" else "/~") <> " " <> show g) $ do
         let Right p' = readPath p
             Right g' = readGlobPath g
         matches p' g' `shouldBe` b
  describe "Data.Json.Path" $ do
     it "Invertible paths" $ do
       mapM_ (\x -> (toText <$> readPath x) `shouldBe` Right (T.pack x) ) inv_paths

     it "Filter json" $ do
       [data'] <- readJsonFile "test/test-files/csexample.json"
       filterJson (const (const True)) data' `shouldBe` data'
       filterJson (const (const False)) data' `shouldBe` (object [])
  --     let isString (String _) = True
  --         isString _ = False
  --     filterJson (\p v -> not (isPrimitive v) || isString v) data' `shouldBe` strings
     it "Filter json with glob" $ do
       [data'] <- readJsonFile "test/test-files/csexample.json"
       let Right everything = readGlobPath ".**"
       filterJsonWithGlob everything data' `shouldBe` data'
       let Right pGlob = readGlobPath ".provider.**"
       filterJsonWithGlob pGlob data' `shouldBe` provider


-- Target json objects

fromJust (Just x) = x

provider = fromJust (decode "{\"provider\":{\"appid\":730,\"name\":\"Counter-Strike: Global Offensive\",\"version\":13512,\"steamid\":\"76561198039225449\",\"timestamp\":1449684054}}" :: Maybe Value)
strings = fromJust (decode "{\"previously\":{\"player\":{\"state\":{},\"name\":\"1000kg\",\"team\":\"T\",\"match_stats\":{},\"steamid\":\"76561198213665501\",\"weapons\":{\"weapon_1\":{\"state\":\"holstered\",\"paintkit\":\"default\",\"name\":\"weapon_glock\",\"type\":\"Pistol\"},\"weapon_0\":{\"state\":\"holstered\",\"paintkit\":\"default\",\"name\":\"weapon_knife_t\",\"type\":\"Knife\"},\"weapon_2\":{\"state\":\"active\",\"paintkit\":\"cu_ak47_rubber\",\"name\":\"weapon_ak47\",\"type\":\"Rifle\"}}}},\"auth\":{\"token\":\"CCWJu64ZV3JHDT8hZc\"},\"player\":{\"name\":\"Hashmush\",\"activity\":\"menu\",\"steamid\":\"76561198039225449\"},\"provider\":{\"name\":\"Counter-Strike: Global Offensive\",\"steamid\":\"76561198039225449\"}}" :: Maybe Value)
